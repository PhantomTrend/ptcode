

####### Data ######

ELECTION_DATA_DIR := ElectionData
FIRST_PREFS_SUMMARY := $(ELECTION_DATA_DIR)/FirstPrefs.csv
$(FIRST_PREFS_SUMMARY): $(ELECTION_DATA_DIR)/HouseFirstPrefsByStateByParty2013.csv \
	$(ELECTION_DATA_DIR)/HouseFirstPrefsByStateByParty2010.csv \
	$(ELECTION_DATA_DIR)/HouseFirstPrefsByStateByParty2007.csv \
	$(ELECTION_DATA_DIR)/HouseFirstPrefsByStateByParty2004.csv
	Rscript WriteFirstPreferenceData.R $@ $^
	
POLL_DATA_DIR := PollingData

$(POLL_DATA_DIR)/NationalDataLong.csv: $(POLL_DATA_DIR)/NationalData.csv
	Rscript WriteNationalDataLong.R $@ $^

$(POLL_DATA_DIR)/StateDataLong.csv: $(POLL_DATA_DIR)/StateData.csv
	Rscript WriteStateDataLong.R $@ $^

MERGED_DATA_FILE := $(POLL_DATA_DIR)/MergedData.csv

$(MERGED_DATA_FILE): $(POLL_DATA_DIR)/NationalDataLong.csv $(POLL_DATA_DIR)/StateDataLong.csv $(FIRST_PREFS_SUMMARY)
	Rscript WriteMergedData.R $@ $^

data-inputs: $(MERGED_DATA_FILE)

PHONY += data-inputs


##### Modelling #####

MODEL_FILE := FittedModel.RData
PARAMETER_MODE_FILE := EstimatedMode.R

OPTIMISE_STEPS ?= 0

$(MODEL_FILE): $(MERGED_DATA_FILE)
	Rscript WriteFittedModel.R $@ $^ $(PARAMETER_MODE_FILE) $(OPTIMISE_STEPS)

fitted-model: $(MODEL_FILE)
PHONY += fitted-model


##### Plots #####

DRAW_PRIMARY_PLOTS := Rscript DrawPrimaryPlots.R
LONG_RUN_PLOTS_DIR := PlotOutputLongrun
RECENT_PLOTS_DIR := PlotOutputRecent

$(LONG_RUN_PLOTS_DIR)/.sentinel: $(MODEL_FILE)
	$(DRAW_PRIMARY_PLOTS) $@ $^ "2000-01-01" "2014-12-01" "ShowPollsters"
	
$(RECENT_PLOTS_DIR)/.sentinel: $(MODEL_FILE)
	$(DRAW_PRIMARY_PLOTS) $@ $^ "2013-01-01" "2014-12-01" "HidePollsters"

plots: $(LONG_RUN_PLOTS_DIR)/.sentinel $(RECENT_PLOTS_DIR)/.sentinel
PHONY += plots


##### Election results #####

ELECTION_RESULTS_DIR := ElectionResults

STATE_SWINGS := $(ELECTION_RESULTS_DIR)/StateSwings.csv
WRITE_STATE_SWINGS := Rscript WriteStateSwings.R
N_STATE_SWING_REPS := 100
LAST_ELECTION_DATE := 2013-09-07

TCP_FLOWS := $(ELECTION_DATA_DIR)/HouseTcpFlowByStateByParty2013.csv
FIRST_PREFS_BY_SEAT := $(ELECTION_DATA_DIR)/HouseFirstPrefsByCandidateByVoteType2013.csv

SEAT_RESULTS_CSV := $(ELECTION_RESULTS_DIR)/SeatResults.csv
WRITE_ELECTION_OUTCOMES := Rscript WriteElectionResults.R
N_SEAT_REPS := 10

$(STATE_SWINGS): $(MODEL_FILE) $(FIRST_PREFS_SUMMARY)
	$(WRITE_STATE_SWINGS) $@ $^ $(N_STATE_SWING_REPS) $(LAST_ELECTION_DATE)

$(SEAT_RESULTS_CSV): $(STATE_SWINGS) $(TCP_FLOWS) $(FIRST_PREFS_BY_SEAT)
	$(WRITE_ELECTION_OUTCOMES) $@ $^ $(N_SEAT_REPS)

election: $(SEAT_RESULTS_CSV)

PHONY += election




