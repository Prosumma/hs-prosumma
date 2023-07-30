{-# LANGUAGE TypeOperators #-}

module Prosumma.Messaging.API (
  API, HealthCheck
) where

import Servant

type HealthCheck = "health" :> Get '[JSON] NoContent 

type API = "v1" :> HealthCheck