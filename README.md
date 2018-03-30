# rangequeryExperiments


This is POC for making range query to database based on history ranges in cache.

Example if there is a range [[1-2),[4-5)] . in cache and a new query for range came with range [1-5]

so query should take only [2-4) . 

