

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


##### Two-party preferred #####

WRITE_2PP := Rscript Write2PartyPreferred.R
TWOPP_OUTPUT_DIR := ElectionResults
TWOPP_FLOW_FILE := ElectionData/HouseTppFlowByStateByParty2013.csv

TWOPP_CSV := $(TWOPP_OUTPUT_DIR)/TwoPartyPreferred.csv

$(TWOPP_CSV): $(MODEL_FILE) $(TWOPP_FLOW_FILE)
	$(WRITE_2PP) $@ $^

two-party-preferred: $(TWOPP_CSV)
PHONY += two-party-preferred


##### Election results and database input #####

ELECTION_RESULTS_DIR := ElectionResults
TPP_OBSERVATIONS_CSV := $(POLL_DATA_DIR)/National2ppData.csv

STATE_SWINGS := $(ELECTION_RESULTS_DIR)/StateSwings.csv
WRITE_STATE_SWINGS := Rscript WriteStateSwings.R
N_STATE_SWING_REPS := 50
LAST_ELECTION_DATE := 2013-09-07

TCP_FLOWS := $(ELECTION_DATA_DIR)/HouseTcpFlowByStateByParty2013.csv
FIRST_PREFS_BY_SEAT := $(ELECTION_DATA_DIR)/HouseFirstPrefsByCandidateByVoteType2013.csv
FIRST_PREFS_BY_STATE := $(ELECTION_DATA_DIR)/HouseFirstPrefsByStateByParty2013.csv
INCUMBENT_SEAT_DATA := $(ELECTION_DATA_DIR)/Incumbents.csv

ELECTION_RESULTS_SENTINEL := $(ELECTION_RESULTS_DIR)/.sentinel
SEAT_RESULTS_CSV := $(ELECTION_RESULTS_DIR)/SeatResults.csv
ELECTION_SUMMARY_JSON := $(ELECTION_RESULTS_DIR)/ElectionSummary.json
WRITE_ELECTION_OUTCOMES := Rscript WriteElectionResults.R
N_SEAT_REPS := 20

$(STATE_SWINGS): $(MODEL_FILE) $(FIRST_PREFS_SUMMARY)
	$(WRITE_STATE_SWINGS) $@ $^ $(N_STATE_SWING_REPS) $(LAST_ELECTION_DATE)

WRITE_PRIMARY_TRENDS := Rscript WritePrimaryTrends.R
PRIMARY_TRENDS := $(ELECTION_RESULTS_DIR)/PrimaryVotes.csv

$(PRIMARY_TRENDS): $(MODEL_FILE)
	$(WRITE_PRIMARY_TRENDS) $@ $^

$(ELECTION_RESULTS_SENTINEL): $(STATE_SWINGS) $(TWOPP_CSV) $(PRIMARY_TRENDS) $(TCP_FLOWS) $(FIRST_PREFS_BY_SEAT) $(FIRST_PREFS_BY_STATE) $(INCUMBENT_SEAT_DATA)
	$(WRITE_ELECTION_OUTCOMES) $@ $^ $(N_SEAT_REPS)

election: $(ELECTION_RESULTS_SENTINEL) $(PRIMARY_TRENDS)

PHONY += election


##### Inputs for web app #####

LOCAL_APP_SENTINEL := .localapp
WEB_APP_SENTINEL := .webapp
WEB_APP_ADDRESS := root@128.199.72.176
WEB_APP_DIR := /tmp

POLLS_FOR_DB := PollsForDb.csv
POLLING_URLS = PollingData/PollingURLs.csv
WRITE_POLL_DATA_FOR_DB := Rscript WritePollingDataForDb.R
$(POLLS_FOR_DB): $(MERGED_DATA_FILE) $(TPP_OBSERVATIONS_CSV) $(POLLING_URLS)
	$(WRITE_POLL_DATA_FOR_DB) $@ $^

$(LOCAL_APP_SENTINEL): $(PRIMARY_TRENDS) $(TWOPP_CSV) $(POLLS_FOR_DB) $(ELECTION_RESULTS_SENTINEL)
	cp $(PRIMARY_TRENDS) $(WEB_APP_DIR)/PrimaryVotes.csv
	cp $(TWOPP_CSV) $(WEB_APP_DIR)/TwoPartyPreferred.csv
	cp $(POLLS_FOR_DB) $(WEB_APP_DIR)/PollsForDb.csv
	cp $(SEAT_RESULTS_CSV) $(WEB_APP_DIR)/SeatResults.csv
	cp $(ELECTION_SUMMARY_JSON) www/ElectionSummary.json
	psql -d ptdata -f makedb.sql
	touch $(LOCAL_APP_SENTINEL)

$(WEB_APP_SENTINEL): $(PRIMARY_TRENDS) $(TWOPP_CSV) $(POLLS_FOR_DB) $(ELECTION_RESULTS_SENTINEL)
	scp $(PRIMARY_TRENDS) $(WEB_APP_ADDRESS):$(WEB_APP_DIR)/PrimaryVotes.csv
	scp $(TWOPP_CSV) $(WEB_APP_ADDRESS):$(WEB_APP_DIR)/TwoPartyPreferred.csv
	scp $(POLLS_FOR_DB) $(WEB_APP_ADDRESS):$(WEB_APP_DIR)/PollsForDb.csv
	scp $(SEAT_RESULTS_CSV) $(WEB_APP_ADDRESS):$(WEB_APP_DIR)/SeatResults.csv
	scp $(ELECTION_SUMMARY_JSON) $(WEB_APP_ADDRESS):/root/ptcode/www/ElectionSummary.json
	ssh root@128.199.72.176 "/root/updatedb.sh"
	touch $(WEB_APP_SENTINEL)

localapp: $(LOCAL_APP_SENTINEL)
webapp: $(WEB_APP_SENTINEL)
app: localapp webapp
PHONY += webapp localapp app



