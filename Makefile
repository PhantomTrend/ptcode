


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

$(POLL_DATA_DIR)/MergedData.csv: $(POLL_DATA_DIR)/NationalDataLong.csv $(POLL_DATA_DIR)/StateDataLong.csv $(ELECTION_DATA_DIR)/FirstPrefs.csv
	Rscript WriteMergedData.R $@ $^



data-inputs: $(POLL_DATA_DIR)/MergedData.csv




