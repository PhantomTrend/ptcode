

####### Data ######

ELECTION_DATA_DIR := ElectionData

$(ELECTION_DATA_DIR)/FirstPrefs.csv: $(ELECTION_DATA_DIR)/HouseFirstPrefsByStateByParty2013.csv \
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

$(MERGED_DATA_FILE): $(POLL_DATA_DIR)/NationalDataLong.csv $(POLL_DATA_DIR)/StateDataLong.csv $(ELECTION_DATA_DIR)/FirstPrefs.csv
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
	$(DRAW_PRIMARY_PLOTS) $@ $^ "2000-01-01" "2014-12-01"
	
$(RECENT_PLOTS_DIR)/.sentinel: $(MODEL_FILE)
	$(DRAW_PRIMARY_PLOTS) $@ $^ "2012-01-01" "2014-12-01"

plots: $(LONG_RUN_PLOTS_DIR)/.sentinel $(RECENT_PLOTS_DIR)/.sentinel
PHONY += plots








