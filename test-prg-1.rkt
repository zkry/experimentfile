#lang experimentfile

experiment DubaiExperiment:

experiment PrioDealsExperiment:
  
experiment PerfMinCTRExperiment:
  
experiment RelativeCTRExperiment:

experiment DiversityExperiment:

experiment PiecewiseLinearExperiment:
  refCTR := 0.001
  alphaDec := 1
  alphaInc := 0.25
  minCampaignCPMFraction := 0.1
  maxCampaignCPMFraction := 0.9

distribution:
  over "UUID"  
  20.0: PiecewiseLinearExperiment
  20.0: PerfMinCTRExperiment
  
