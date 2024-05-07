module Test.MySolutions where

import Prelude
import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter ((_ == street) <<< _.address.street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not <<< null <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq areSameEntry
  where
  areSameEntry e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
