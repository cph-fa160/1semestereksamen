-- Opgave 2.1 – Oprette skemaet for Bilbasen

CREATE DATABASE Bilbasen;
USE Bilbasen;

-- Forhandler (invariant)
CREATE TABLE dealer (
  dealer_id      VARCHAR(20) PRIMARY KEY,
  seller_name    VARCHAR(255),
  seller_address VARCHAR(255),
  seller_cvr     VARCHAR(20)
);

-- Bil (invariant)
CREATE TABLE car (
  carid      VARCHAR(20) PRIMARY KEY,
  makemodel  VARCHAR(255),
  props      VARCHAR(255),
  link       TEXT,
  dealer_id  VARCHAR(20),
  CONSTRAINT car_ibfk_1
    FOREIGN KEY (dealer_id) REFERENCES dealer(dealer_id)
);

-- Observationer (tidsserie)
CREATE TABLE car_observation (
  id                INT AUTO_INCREMENT PRIMARY KEY,
  carid             VARCHAR(20),
  price             VARCHAR(50),
  description_clean TEXT,
  location          VARCHAR(255),
  scrapedate        DATE,
  sold              TINYINT(1) NOT NULL DEFAULT 0,
  CONSTRAINT car_obs_fk
    FOREIGN KEY (carid) REFERENCES car(carid)
);
