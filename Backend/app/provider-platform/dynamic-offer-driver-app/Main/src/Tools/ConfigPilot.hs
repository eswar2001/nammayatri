{-# OPTIONS_GHC -Wno-deprecations #-}

module Tools.ConfigPilot where

import qualified Data.Aeson as A
import qualified Domain.Types.DriverPoolConfig as DTD
import qualified Domain.Types.MerchantMessage as DTM
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.MerchantPushNotification as DTPN
import qualified Domain.Types.PayoutConfig as DTP
import qualified Domain.Types.RideRelatedNotificationConfig as DTRN
import qualified Domain.Types.TransporterConfig as DTT
-- import qualified Storage.Queries.UiDriverConfig as QUiC
import EulerHS.Prelude hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
-- import qualified Storage.CachedQueries.UiDriverConfig as QUiConfig

import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as SCMDPC
import qualified Storage.CachedQueries.Merchant.MerchantMessage as SCMM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as SCMMPN
import qualified Storage.CachedQueries.Merchant.PayoutConfig as SCMP
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCMTC
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SCR
import qualified Storage.Queries.DriverPoolConfig as SCMD
import qualified Storage.Queries.MerchantMessage as SQM
import qualified Storage.Queries.MerchantPushNotification as SQMPN
import qualified Storage.Queries.PayoutConfig as SCP
import qualified Storage.Queries.RideRelatedNotificationConfig as SQR
import qualified Storage.Queries.TransporterConfig as SCMT

returnConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LYT.ConfigType -> Id MerchantOperatingCity -> m LYT.TableDataResp
returnConfigs cfgType merchantOpCityId = do
  case cfgType of
    LYT.DriverPoolConfig -> do
      driverPoolCfg <- SCMDPC.findAllByMerchantOpCityId merchantOpCityId (Just []) Nothing
      return LYT.TableDataResp {configs = map A.toJSON driverPoolCfg}
    LYT.TransporterConfig -> do
      transporterCfg <- SCMTC.getTransporterConfigFromDB merchantOpCityId
      return LYT.TableDataResp {configs = map A.toJSON (maybeToList transporterCfg)}
    LYT.PayoutConfig -> do
      payoutCfg <- SCMP.findAllByMerchantOpCityId merchantOpCityId (Just [])
      return LYT.TableDataResp {configs = map A.toJSON payoutCfg}
    LYT.RideRelatedNotificationConfig -> do
      rideRelatedNotificationCfg <- SCR.findAllByMerchantOperatingCityId merchantOpCityId (Just [])
      return LYT.TableDataResp {configs = map A.toJSON rideRelatedNotificationCfg}
    LYT.MerchantMessage -> do
      merchantMessage <- SCMM.findAllByMerchantOpCityId merchantOpCityId (Just [])
      return LYT.TableDataResp {configs = map A.toJSON merchantMessage}
    LYT.MerchantPushNotification -> do
      merchantPushNotification <- SCMMPN.findAllByMerchantOpCityId merchantOpCityId (Just [])
      return LYT.TableDataResp {configs = map A.toJSON merchantPushNotification}
    _ -> throwError $ InvalidRequest "Unsupported config type."

handleConfigDBUpdate :: (BeamFlow m r, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> LYT.ConcludeReq -> [A.Value] -> m ()
handleConfigDBUpdate merchantOpCityId concludeReq baseLogics = do
  case concludeReq.domain of
    LYT.DRIVER_CONFIG LYT.DriverPoolConfig -> do
      driverPoolCfg <- SCMDPC.findAllByMerchantOpCityId merchantOpCityId (Just []) Nothing
      let configWrapper :: [LYT.Config DTD.DriverPoolConfig] = convertToConfigWrapper driverPoolCfg
      cfgs :: [LYT.Config DTD.DriverPoolConfig] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DTD.DriverPoolConfig] = getConfigsToUpdate configWrapper cfgs
      mapM_ SCMD.updateByPrimaryKey configsToUpdate
      SCMDPC.clearCache merchantOpCityId
    LYT.DRIVER_CONFIG LYT.TransporterConfig -> do
      transporterCfg <- SCMTC.getTransporterConfigFromDB merchantOpCityId
      let configWrapper :: [LYT.Config DTT.TransporterConfig] = convertToConfigWrapper (maybeToList transporterCfg)
      cfgs :: [LYT.Config DTT.TransporterConfig] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DTT.TransporterConfig] = getConfigsToUpdate configWrapper cfgs
      mapM_ SCMT.update configsToUpdate
      SCMTC.clearCache merchantOpCityId
    LYT.DRIVER_CONFIG LYT.PayoutConfig -> do
      payoutCfg <- SCMP.findAllByMerchantOpCityId merchantOpCityId (Just [])
      let configWrapper :: [LYT.Config DTP.PayoutConfig] = convertToConfigWrapper payoutCfg
      cfgs :: [LYT.Config DTP.PayoutConfig] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DTP.PayoutConfig] = getConfigsToUpdate configWrapper cfgs
      mapM_ SCP.updateByPrimaryKey configsToUpdate
      SCMP.clearCacheById merchantOpCityId -- TODO use clearConfigCache
    LYT.DRIVER_CONFIG LYT.RideRelatedNotificationConfig -> do
      rideRelatedNotificationCfg <- SCR.findAllByMerchantOperatingCityId merchantOpCityId (Just [])
      let configWrapper :: [LYT.Config DTRN.RideRelatedNotificationConfig] = convertToConfigWrapper rideRelatedNotificationCfg
      cfgs :: [LYT.Config DTRN.RideRelatedNotificationConfig] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DTRN.RideRelatedNotificationConfig] = getConfigsToUpdate configWrapper cfgs
      mapM_ SQR.updateByPrimaryKey configsToUpdate
    -- TODO add clear cache
    LYT.DRIVER_CONFIG LYT.MerchantMessage -> do
      merchantMessage <- SCMM.findAllByMerchantOpCityId merchantOpCityId (Just [])
      let configWrapper :: [LYT.Config DTM.MerchantMessage] = convertToConfigWrapper merchantMessage
      cfgs :: [LYT.Config DTM.MerchantMessage] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DTM.MerchantMessage] = getConfigsToUpdate configWrapper cfgs
      mapM_ SQM.updateByPrimaryKey configsToUpdate
      SCMM.clearCacheById merchantOpCityId
    LYT.DRIVER_CONFIG LYT.MerchantPushNotification -> do
      merchantPushNotification <- SCMMPN.findAllByMerchantOpCityId merchantOpCityId (Just [])
      let configWrapper :: [LYT.Config DTPN.MerchantPushNotification] = convertToConfigWrapper merchantPushNotification
      cfgs :: [LYT.Config DTPN.MerchantPushNotification] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DTPN.MerchantPushNotification] = getConfigsToUpdate configWrapper cfgs
      mapM_ SQMPN.updateByPrimaryKey configsToUpdate
      SCMMPN.clearCacheById merchantOpCityId
    _ -> throwError $ InvalidRequest $ "Logic Domain not supported" <> show concludeReq.domain
  where
    convertToConfigWrapper :: [a] -> [LYT.Config a]
    convertToConfigWrapper configs =
      zipWith
        (\id cfg -> cfg {LYT.identifier = id})
        [0 ..]
        (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) configs)

    applyPatchToConfig :: forall b m. (FromJSON b, MonadFlow m, ToJSON b) => [LYT.Config b] -> m [LYT.Config b]
    applyPatchToConfig configWrapper = do
      patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
      mapM
        ( \resp ->
            case (A.fromJSON (resp.result) :: A.Result (LYT.Config b)) of
              A.Success cfg -> pure cfg
              A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to the config. " <> show e
        )
        patchedConfigs

    getConfigsToUpdate :: Eq a => [LYT.Config a] -> [LYT.Config a] -> [a]
    getConfigsToUpdate configWrapper cfgs =
      let sortedCfgs = sortOn LYT.identifier cfgs
       in catMaybes $
            zipWith
              ( \cfg1 cfg2 ->
                  if cfg1.identifier == cfg2.identifier && cfg1.config /= cfg2.config
                    then Just cfg2.config
                    else Nothing
              )
              configWrapper
              sortedCfgs
