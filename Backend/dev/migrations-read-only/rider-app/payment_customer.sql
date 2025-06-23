CREATE TABLE atlas_app.payment_customer ();

ALTER TABLE atlas_app.payment_customer ADD COLUMN client_auth_token text ;
ALTER TABLE atlas_app.payment_customer ADD COLUMN client_auth_token_expiry timestamp with time zone ;
ALTER TABLE atlas_app.payment_customer ADD COLUMN customer_id text NOT NULL;
ALTER TABLE atlas_app.payment_customer ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payment_customer ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payment_customer ADD PRIMARY KEY ( customer_id);
