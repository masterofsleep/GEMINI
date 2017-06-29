# ------------------------------ Detail check ----------------------------------
library(gemini)
lib.pa()

# in clinical check, the admit date/time does not matched for the MRN
# EncID.new: 11559669 MRN: 1293836
# EncID.new: 11463970 MRN: 2585913
link <- readg(SMH, LINKLIST_NEWHASH)
dad <- readg(smh, dad)

link[EncID.new=="559669"]
dad[EncID.new=="11559669"]
smh.lab <- readg(smh, labs)
smh.lab[EncID.new=="11559669"]
#exists in system, mistake from last time