module SampleRunner where

data SampleRunner = SampleRunner { getStopFunction :: IO (), getRunning :: IO SampleRunner }
emptySampleRunner = SampleRunner (return ()) (return emptySampleRunner)

