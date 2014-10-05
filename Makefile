

POLL_DATA_DIR := PollingData

$(POLL_DATA_DIR)/NationalDataLong.csv: $(POLL_DATA_DIR)/NationalData.csv
	Rscript WriteNationalDataLong.R $@ $^

$(POLL_DATA_DIR)/StateDataLong.csv: $(POLL_DATA_DIR)/StateData.csv
	Rscript WriteStateDataLong.R $@ $^

$(POLL_DATA_DIR)/MergedData.csv: $(POLL_DATA_DIR)/NationalDataLong.csv $(POLL_DATA_DIR)/StateDataLong.csv
	Rscript WriteMergedData.R $@ $^

data-inputs: $(POLL_DATA_DIR)/MergedData.csv




