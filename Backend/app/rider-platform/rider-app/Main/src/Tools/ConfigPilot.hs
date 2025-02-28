{-# OPTIONS_GHC -Wno-deprecations #-}

module Tools.ConfigPilot where

import qualified Data.Aeson as A
import qualified Domain.Types.FRFSConfig as DFRFS
import qualified Domain.Types.MerchantConfig as DTM
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.MerchantPushNotification as DTPN
import qualified Domain.Types.PayoutConfig as DTP
import qualified Domain.Types.RideRelatedNotificationConfig as DTRN
import qualified Domain.Types.RiderConfig as DTR
import EulerHS.Prelude hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
-- import qualified Storage.CachedQueries.UiRiderConfig as UIRC

import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types as LYTU
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.FRFSConfig as SCFRFS
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as SCMMPN
import qualified Storage.CachedQueries.Merchant.PayoutConfig as SCMPC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.MerchantConfig as SCMC
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SCRRN
import qualified Storage.Queries.FRFSConfig as SQFRFS
import qualified Storage.Queries.MerchantConfig as SQMC
import qualified Storage.Queries.MerchantPushNotification as SQMPN
import qualified Storage.Queries.PayoutConfig as SQPC
import qualified Storage.Queries.RideRelatedNotificationConfig as SQRRN
import qualified Storage.Queries.RiderConfig as SQR

returnConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LYTU.ConfigType -> Id MerchantOperatingCity -> m LYTU.TableDataResp
returnConfigs cfgType merchantOpCityId = do
  case cfgType of
    LYTU.RiderConfig -> do
      riderCfg <- QRC.findByMerchantOperatingCityId merchantOpCityId (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON (maybeToList riderCfg)}
    LYTU.PayoutConfig -> do
      payoutCfg <- SCMPC.findAllByMerchantOpCityId merchantOpCityId (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON payoutCfg}
    LYTU.RideRelatedNotificationConfig -> do
      rideRelatedNotificationCfg <- SCRRN.findAllByMerchantOperatingCityId merchantOpCityId (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON rideRelatedNotificationCfg}
    LYTU.MerchantConfig -> do
      merchantCfg <- SCMC.findAllByMerchantOperatingCityId merchantOpCityId (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON merchantCfg}
    LYTU.MerchantPushNotification -> do
      merchantPushNotification <- SCMMPN.findAllByMerchantOpCityId merchantOpCityId (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON merchantPushNotification}
    LYTU.FRFSConfig -> do
      frfsConfig <- SCFRFS.findByMerchantOperatingCityId merchantOpCityId (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON (maybeToList frfsConfig)}
    _ -> throwError $ InvalidRequest "Unsupported config type."

handleConfigDBUpdate :: (BeamFlow m r, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> LYTU.ConcludeReq -> [A.Value] -> m ()
handleConfigDBUpdate merchantOpCityId concludeReq baseLogics = do
  case concludeReq.domain of
    LYTU.RIDER_CONFIG LYTU.RiderConfig -> do
      riderCfg <- QRC.findByMerchantOperatingCityId merchantOpCityId (Just [])
      let configWrapper :: [LYTU.Config DTR.RiderConfig] = convertToConfigWrapper (maybeToList riderCfg)
      cfgs :: [LYTU.Config DTR.RiderConfig] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DTR.RiderConfig] = getConfigsToUpdate configWrapper cfgs
      mapM_ SQR.updateByPrimaryKey configsToUpdate
      QRC.clearCache merchantOpCityId
    LYTU.RIDER_CONFIG LYTU.PayoutConfig -> do
      payoutCfg <- SCMPC.findAllByMerchantOpCityId merchantOpCityId (Just [])
      let configWrapper :: [LYTU.Config DTP.PayoutConfig] = convertToConfigWrapper payoutCfg
      cfgs :: [LYTU.Config DTP.PayoutConfig] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DTP.PayoutConfig] = getConfigsToUpdate configWrapper cfgs
      mapM_ SQPC.updateByPrimaryKey configsToUpdate
    -- TODO add clearCache
    LYTU.RIDER_CONFIG LYTU.RideRelatedNotificationConfig -> do
      rideRelatedNotificationCfg <- SCRRN.findAllByMerchantOperatingCityId merchantOpCityId (Just [])
      let configWrapper :: [LYTU.Config DTRN.RideRelatedNotificationConfig] = convertToConfigWrapper rideRelatedNotificationCfg
      cfgs :: [LYTU.Config DTRN.RideRelatedNotificationConfig] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DTRN.RideRelatedNotificationConfig] = getConfigsToUpdate configWrapper cfgs
      mapM_ SQRRN.updateByPrimaryKey configsToUpdate
    -- TODO add clearCache
    LYTU.RIDER_CONFIG LYTU.MerchantConfig -> do
      merchantCfg <- SCMC.findAllByMerchantOperatingCityId merchantOpCityId (Just [])
      let configWrapper :: [LYTU.Config DTM.MerchantConfig] = convertToConfigWrapper merchantCfg
      cfgs :: [LYTU.Config DTM.MerchantConfig] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DTM.MerchantConfig] = getConfigsToUpdate configWrapper cfgs
      mapM_ SQMC.updateByPrimaryKey configsToUpdate
      SCMC.clearCache merchantOpCityId
    LYTU.RIDER_CONFIG LYTU.MerchantPushNotification -> do
      merchantPushNotification <- SCMMPN.findAllByMerchantOpCityId merchantOpCityId (Just [])
      let configWrapper :: [LYTU.Config DTPN.MerchantPushNotification] = convertToConfigWrapper merchantPushNotification
      cfgs :: [LYTU.Config DTPN.MerchantPushNotification] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DTPN.MerchantPushNotification] = getConfigsToUpdate configWrapper cfgs
      mapM_ SQMPN.updateByPrimaryKey configsToUpdate
    -- TODO add clearCache
    LYTU.RIDER_CONFIG LYTU.FRFSConfig -> do
      frfsConfig <- SCFRFS.findByMerchantOperatingCityId merchantOpCityId (Just [])
      let configWrapper :: [LYTU.Config DFRFS.FRFSConfig] = convertToConfigWrapper (maybeToList frfsConfig)
      cfgs :: [LYTU.Config DFRFS.FRFSConfig] <- applyPatchToConfig configWrapper
      let configsToUpdate :: [DFRFS.FRFSConfig] = getConfigsToUpdate configWrapper cfgs
      mapM_ SQFRFS.updateByPrimaryKey configsToUpdate
      SCFRFS.clearCache merchantOpCityId
    _ -> throwError $ InvalidRequest $ "Logic Domain not supported" <> show concludeReq.domain
  where
    convertToConfigWrapper :: [a] -> [LYTU.Config a]
    convertToConfigWrapper configs =
      zipWith
        (\id cfg -> cfg {LYTU.identifier = id})
        [0 ..]
        (map (\cfg -> LYTU.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) configs)

    applyPatchToConfig :: forall b m. (FromJSON b, MonadFlow m, ToJSON b) => [LYTU.Config b] -> m [LYTU.Config b]
    applyPatchToConfig configWrapper = do
      patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
      mapM
        ( \resp ->
            case (A.fromJSON (resp.result) :: A.Result (LYTU.Config b)) of
              A.Success cfg -> pure cfg
              A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to the config. " <> show e
        )
        patchedConfigs

    getConfigsToUpdate :: Eq a => [LYTU.Config a] -> [LYTU.Config a] -> [a]
    getConfigsToUpdate configWrapper cfgs =
      let sortedCfgs = sortOn LYTU.identifier cfgs
       in catMaybes $
            zipWith
              ( \cfg1 cfg2 ->
                  if cfg1.identifier == cfg2.identifier && cfg1.config /= cfg2.config
                    then Just cfg2.config
                    else Nothing
              )
              configWrapper
              sortedCfgs
