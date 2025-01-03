module Prosumma.Types.IP (
  IP,
  ipFromSockAddr
) where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Net.Types
import Network.Socket
import Prosumma.Textual
import RIO

import qualified Net.IP as IP


ipFromSockAddr :: SockAddr -> Maybe IP
ipFromSockAddr (SockAddrInet _ hostAddress) = let (t1, t2, t3, t4) = hostAddressToTuple hostAddress in Just $ IP.ipv4 t1 t2 t3 t4
ipFromSockAddr (SockAddrInet6 _ _ hostAddress _) = let (t1, t2, t3, t4, t5, t6, t7, t8) = hostAddress6ToTuple hostAddress in Just $ IP.ipv6 t1 t2 t3 t4 t5 t6 t7 t8
ipFromSockAddr _ = Nothing

instance FromText IP where
  fromText = IP.decode

instance ToText IP where
  toText = IP.encode

instance FromField IP where
  fromField = fromFieldTextual "IP"

instance ToField IP where
  toField = toFieldTextual
