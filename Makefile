.SECONDEXPANSION:

####### Data ######

WORKING_DIR := Working
ELECTION_DATA_DIR := ElectionData
FIRST_PREFS_SUMMARY := $(WORKING_DIR)/FirstPrefs.csv
$(FIRST_PREFS_SUMMARY): $(ELECTION_DATA_DIR)/HouseFirstPrefsByStateByParty2013.csv \
	$(ELECTION_DATA_DIR)/HouseFirstPrefsByStateByParty2010.csv \
	$(ELECTION_DATA_DIR)/HouseFirstPrefsByStateByParty2007.csv \
	$(ELECTION_DATA_DIR)/HouseFirstPrefsByStateByParty2004.csv
	Rscript WriteFirstPreferenceData.R $@ $^
	
POLL_DATA_DIR := PollingData

$(WORKING_DIR)/NationalDataLong.csv: $(POLL_DATA_DIR)/NationalData.csv
	Rscript WriteNationalDataLong.R $@ $^

$(WORKING_DIR)/StateDataLong.csv: $(POLL_DATA_DIR)/StateData.csv
	Rscript WriteStateDataLong.R $@ $^

MERGED_DATA_FILE := $(WORKING_DIR)/MergedData.csv

$(MERGED_DATA_FILE): $(WORKING_DIR)/NationalDataLong.csv $(WORKING_DIR)/StateDataLong.csv $(FIRST_PREFS_SUMMARY)
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
SEAT_RESULTS_DIR := $(ELECTION_RESULTS_DIR)/Seats
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
RSS_FILE := $(ELECTION_RESULTS_DIR)/rss.xml
WRITE_ELECTION_OUTCOMES := Rscript WriteElectionResults.R
N_SEAT_REPS := 25

$(STATE_SWINGS): $(MODEL_FILE) $(FIRST_PREFS_SUMMARY)
	$(WRITE_STATE_SWINGS) $@ $^ $(N_STATE_SWING_REPS) $(LAST_ELECTION_DATE)

WRITE_PRIMARY_TRENDS := Rscript WritePrimaryTrends.R
PRIMARY_TRENDS := $(ELECTION_RESULTS_DIR)/PrimaryVotes.csv

$(PRIMARY_TRENDS): $(MODEL_FILE)
	$(WRITE_PRIMARY_TRENDS) $@ $^


ELECTORATE_NAMES := Canberra Fraser Banks Barton Bennelong Berowra Blaxland Bradfield Calare Charlton Chifley Cook Cowper Cunningham Dobell Eden-Monaro Farrer Fowler Gilmore Grayndler Greenway Hughes Hume Hunter Kingsford_Smith Lindsay Lyne Macarthur Mackellar Macquarie McMahon Mitchell New_England Newcastle North_Sydney Page Parkes Parramatta Paterson Reid Richmond Riverina Robertson Shortland Sydney Throsby Warringah Watson Wentworth Werriwa Lingiari Solomon Blair Bonner Bowman Brisbane Capricornia Dawson Dickson Fadden Fairfax Fisher Flynn Forde Griffith Groom Herbert Hinkler Kennedy Leichhardt Lilley Longman Maranoa McPherson Moncrieff Moreton Oxley Petrie Rankin Ryan Wide_Bay Wright Adelaide Barker Boothby Grey Hindmarsh Kingston Makin Mayo Port_Adelaide Sturt Wakefield Bass Braddon Denison Franklin Lyons Aston Ballarat Batman Bendigo Bruce Calwell Casey Chisholm Corangamite Corio Deakin Dunkley Flinders Gellibrand Gippsland Goldstein Gorton Higgins Holt Hotham Indi Isaacs Jagajaga Kooyong La_Trobe Lalor Mallee Maribyrnong McEwen McMillan Melbourne Melbourne_Ports Menzies Murray Scullin Wannon Wills Brand Canning Cowan Curtin Durack Forrest Fremantle Hasluck Moore O_Connor Pearce Perth Stirling Swan Tangney


define SEAT_RULES

$(SEAT_RESULTS_DIR)/$(SEAT).csv: $(STATE_SWINGS) $(TWOPP_CSV) $(PRIMARY_TRENDS) $(TCP_FLOWS) $(FIRST_PREFS_BY_SEAT) $(FIRST_PREFS_BY_STATE)
	Rscript WriteSingleSeatResults.R $$@ $$^ $(SEAT) $(N_SEAT_REPS)

SEAT_RESULTS += $(SEAT_RESULTS_DIR)/$(SEAT).csv 

endef # SEAT_RULES

$(foreach SEAT, $(ELECTORATE_NAMES), $(eval $(call SEAT_RULES)))

SEAT_RESULTS_SENTINEL = $(SEAT_RESULTS_DIR)/.sentinel
$(SEAT_RESULTS_SENTINEL): $(SEAT_RESULTS)
	touch $(SEAT_RESULTS_SENTINEL)

$(ELECTION_RESULTS_SENTINEL): $(SEAT_RESULTS_SENTINEL) $(FIRST_PREFS_BY_SEAT) $(INCUMBENT_SEAT_DATA) $(TWOPP_CSV) $(PRIMARY_TRENDS)
	$(WRITE_ELECTION_OUTCOMES) $@ $^

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
	cp $(RSS_FILE) www/rss.xml
	psql -d ptdata -f makedb.sql
	touch $(LOCAL_APP_SENTINEL)

$(WEB_APP_SENTINEL): $(PRIMARY_TRENDS) $(TWOPP_CSV) $(POLLS_FOR_DB) $(ELECTION_RESULTS_SENTINEL)
	scp -i ~/.ssh/id_digitalocean $(PRIMARY_TRENDS) $(WEB_APP_ADDRESS):$(WEB_APP_DIR)/PrimaryVotes.csv
	scp -i ~/.ssh/id_digitalocean $(TWOPP_CSV) $(WEB_APP_ADDRESS):$(WEB_APP_DIR)/TwoPartyPreferred.csv
	scp -i ~/.ssh/id_digitalocean $(POLLS_FOR_DB) $(WEB_APP_ADDRESS):$(WEB_APP_DIR)/PollsForDb.csv
	scp -i ~/.ssh/id_digitalocean $(SEAT_RESULTS_CSV) $(WEB_APP_ADDRESS):$(WEB_APP_DIR)/SeatResults.csv
	scp -i ~/.ssh/id_digitalocean $(ELECTION_SUMMARY_JSON) $(WEB_APP_ADDRESS):/root/ptcode/www/ElectionSummary.json
	scp -i ~/.ssh/id_digitalocean $(RSS_FILE) $(WEB_APP_ADDRESS):/root/ptcode/www/rss.xml
	ssh -i ~/.ssh/id_digitalocean root@128.199.72.176 "/root/updatedb.sh"
	touch $(WEB_APP_SENTINEL)

localapp: $(LOCAL_APP_SENTINEL)
webapp: $(WEB_APP_SENTINEL)
app: localapp webapp
PHONY += webapp localapp app

print-%:: ; @echo $* is $($*)
.%:: $$($$*) ;
