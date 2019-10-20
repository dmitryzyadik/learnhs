eitherToMaybe :: Either a -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing