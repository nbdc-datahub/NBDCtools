# Create BIDS sidecar from metadata

Generates a Brain Imaging Data Structure (BIDS) JSON sidecar using
metadata tables (data dictionary and levels) without requiring the
underlying data.

## Usage

``` r
create_bids_sidecar_metadata(
  dd,
  levels,
  vars = NULL,
  tables = NULL,
  metadata_description = "Dataset exported using NBDCtools",
  path_out = NULL,
  pretty = TRUE
)
```

## Arguments

- dd:

  tibble, Data dictionary metadata, see
  [`get_dd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_dd.md)
  or provide a custom data dictionary. The custom data dictionary must
  have same structure as the official data dictionaries.

- levels:

  tibble, Levels metadata corresponding to `dd`, see
  [`get_levels()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_levels.md),
  or provide a custom levels table. The custom levels table must have
  same structure as the official levels tables.

- vars:

  character vector, variable names to include. `vars` and `tables`
  cannot be both `NULL`.

- tables:

  character vector, table names to include.

- metadata_description:

  string, the description of the metadata

- path_out:

  character. the path to the output file. If `NULL`, the function will
  return the json object.

- pretty:

  logical. Whether to pretty print the json.

## Value

Either a JSON string (when `path_out` is `NULL`) or the output file path
(invisibly) after writing the JSON sidecar to disk.

## Details

- If you have a labelled dataset, and want to create data specific BIDS
  sidecar with variable levels from the data, please use
  [`create_bids_sidecar_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_bids_sidecar_data.md).

- If you want to create a BIDS sidecar without the underlying data,
  please use `create_bids_sidecar_metadata()`.

## See also

[`create_bids_sidecar_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_bids_sidecar_data.md)

## Examples

``` r
create_bids_sidecar_metadata(
  dd = get_dd("abcd"),
  levels = get_levels("abcd"),
  tables = c("ph_y_mctq")
)
#> {
#>   "MeasurementToolMetadata": {
#>     "Description": ["Dataset exported using NBDCtools"]
#>   },
#>   "ph_y_mctq_dtt": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth]: Date/time of collection start"],
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq_age": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth]: Youth's age at data collection (note: if table-specific date/time is missing, the age at visit start is used)"],
#>     "Units": ["years (yr)"],
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq_adm": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth]: Method of administration"],
#>     "Levels": {
#>       "1": ["RA-Administered"],
#>       "2": ["Youth Self-Administered"],
#>       "3": ["Combination"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_001": {
#>     "Description": ["Free Days: I go to bed at: What part of the day?"],
#>     "Levels": {
#>       "1": ["Morning (4:00 AM to 11:59 AM)"],
#>       "2": ["Afternoon (12:00 PM Noon to 4:59 PM)"],
#>       "3": ["Evening (5:00 PM to 8:59 PM)"],
#>       "4": ["Nighttime (9:00 PM to 3:59 AM)"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_001__01a": {
#>     "Description": ["Free Days: I go to bed at - 4am to 4pm: Hour"],
#>     "Levels": {
#>       "1": ["4 AM"],
#>       "10": ["1 PM"],
#>       "11": ["2 PM"],
#>       "12": ["3 PM"],
#>       "13": ["4 PM"],
#>       "2": ["5 AM"],
#>       "3": ["6 AM"],
#>       "4": ["7 AM"],
#>       "5": ["8 AM"],
#>       "6": ["9 AM"],
#>       "7": ["10 AM"],
#>       "8": ["11 AM"],
#>       "9": ["12 PM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_001__01b": {
#>     "Description": ["Free Days: I go to bed at - 5pm to 3am: Hour"],
#>     "Levels": {
#>       "1": ["5 PM"],
#>       "10": ["2 AM"],
#>       "11": ["3 AM"],
#>       "2": ["6 PM"],
#>       "3": ["7 PM"],
#>       "4": ["8 PM"],
#>       "5": ["9 PM"],
#>       "6": ["10 PM"],
#>       "7": ["11 PM"],
#>       "8": ["12 AM"],
#>       "9": ["1 AM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_001__02": {
#>     "Description": ["Free Days: I go to bed at: Minutes"],
#>     "Levels": {
#>       "1": ["0 minutes"],
#>       "10": ["45 minutes"],
#>       "11": ["50 minutes"],
#>       "12": ["55 minutes"],
#>       "2": ["5 minutes"],
#>       "3": ["10 minutes"],
#>       "4": ["15 minutes"],
#>       "5": ["20 minutes"],
#>       "6": ["25 minutes"],
#>       "7": ["30 minutes"],
#>       "8": ["35 minutes"],
#>       "9": ["40 minutes"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_002": {
#>     "Description": ["Free Days: I actually start trying to fall asleep at: What part of the day?"],
#>     "Levels": {
#>       "1": ["Morning (4:00 AM to 11:59 AM)"],
#>       "2": ["Afternoon (12:00 PM Noon to 4:59 PM)"],
#>       "3": ["Evening (5:00 PM to 8:59 PM)"],
#>       "4": ["Nighttime (9:00 PM to 3:59 AM)"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_002__01a": {
#>     "Description": ["Free Days: I actually start trying to fall asleep at - 4am to 4pm: Hour"],
#>     "Levels": {
#>       "1": ["4 AM"],
#>       "10": ["1 PM"],
#>       "11": ["2 PM"],
#>       "12": ["3 PM"],
#>       "13": ["4 PM"],
#>       "2": ["5 AM"],
#>       "3": ["6 AM"],
#>       "4": ["7 AM"],
#>       "5": ["8 AM"],
#>       "6": ["9 AM"],
#>       "7": ["10 AM"],
#>       "8": ["11 AM"],
#>       "9": ["12 PM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_002__01b": {
#>     "Description": ["Free Days: I actually start trying to fall asleep at - 5pm to 3am: Hour"],
#>     "Levels": {
#>       "1": ["5 PM"],
#>       "10": ["2 AM"],
#>       "11": ["3 AM"],
#>       "2": ["6 PM"],
#>       "3": ["7 PM"],
#>       "4": ["8 PM"],
#>       "5": ["9 PM"],
#>       "6": ["10 PM"],
#>       "7": ["11 PM"],
#>       "8": ["12 AM"],
#>       "9": ["1 AM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_002__02": {
#>     "Description": ["Free Days: I actually start trying to fall asleep at: Minutes"],
#>     "Levels": {
#>       "1": ["0 minutes"],
#>       "10": ["45 minutes"],
#>       "11": ["50 minutes"],
#>       "12": ["55 minutes"],
#>       "2": ["5 minutes"],
#>       "3": ["10 minutes"],
#>       "4": ["15 minutes"],
#>       "5": ["20 minutes"],
#>       "6": ["25 minutes"],
#>       "7": ["30 minutes"],
#>       "8": ["35 minutes"],
#>       "9": ["40 minutes"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_003": {
#>     "Description": ["Free Days: I need ________ minutes to fall asleep."],
#>     "Levels": {
#>       "0": ["0"],
#>       "9": ["9"],
#>       "10": ["10"],
#>       "11": ["15"],
#>       "12": ["20"],
#>       "13": ["25"],
#>       "14": ["30"],
#>       "15": ["40"],
#>       "16": ["50"],
#>       "17": ["1 hour"],
#>       "18": ["1 hour 15 minute"],
#>       "1": ["1"],
#>       "19": ["1 hour 30 minute"],
#>       "20": ["1 hour 45 minute"],
#>       "21": ["2 hours"],
#>       "22": ["3 hours"],
#>       "23": ["4 hours"],
#>       "2": ["2"],
#>       "3": ["3"],
#>       "4": ["4"],
#>       "5": ["5"],
#>       "6": ["6"],
#>       "7": ["7"],
#>       "8": ["8"]
#>     },
#>     "Units": ["minutes (min)"],
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_004": {
#>     "Description": ["Free Days: After falling asleep, I wake up ____ times during the night."],
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_004__01": {
#>     "Description": ["Free Days: Altogether, these awakenings last ______minutes."],
#>     "Levels": {
#>       "0": ["0"],
#>       "9": ["9"],
#>       "10": ["10"],
#>       "11": ["15"],
#>       "12": ["20"],
#>       "13": ["25"],
#>       "14": ["30"],
#>       "15": ["40"],
#>       "16": ["50"],
#>       "17": ["1 hour"],
#>       "18": ["1 hour 15 minute"],
#>       "1": ["1"],
#>       "19": ["1 hour 30 minute"],
#>       "20": ["1 hour 45 minute"],
#>       "21": ["2 hours"],
#>       "22": ["3 hours"],
#>       "23": ["4 hours"],
#>       "2": ["2"],
#>       "3": ["3"],
#>       "4": ["4"],
#>       "5": ["5"],
#>       "6": ["6"],
#>       "7": ["7"],
#>       "8": ["8"]
#>     },
#>     "Units": ["minutes (min)"],
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_005": {
#>     "Description": ["Free Days: I wake up at: What part of the day?"],
#>     "Levels": {
#>       "1": ["Morning (4:00 AM to 11:59 AM)"],
#>       "2": ["Afternoon (12:00 PM Noon to 4:59 PM)"],
#>       "3": ["Evening (5:00 PM to 8:59 PM)"],
#>       "4": ["Nighttime (9:00 PM to 3:59 AM)"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_005__01a": {
#>     "Description": ["Free Days: I wake up at - 4am to 4pm: Hour"],
#>     "Levels": {
#>       "1": ["4 AM"],
#>       "10": ["1 PM"],
#>       "11": ["2 PM"],
#>       "12": ["3 PM"],
#>       "13": ["4 PM"],
#>       "2": ["5 AM"],
#>       "3": ["6 AM"],
#>       "4": ["7 AM"],
#>       "5": ["8 AM"],
#>       "6": ["9 AM"],
#>       "7": ["10 AM"],
#>       "8": ["11 AM"],
#>       "9": ["12 PM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_005__01b": {
#>     "Description": ["Free Days: I wake up at - 5pm to 3am: Hour"],
#>     "Levels": {
#>       "1": ["5 PM"],
#>       "10": ["2 AM"],
#>       "11": ["3 AM"],
#>       "2": ["6 PM"],
#>       "3": ["7 PM"],
#>       "4": ["8 PM"],
#>       "5": ["9 PM"],
#>       "6": ["10 PM"],
#>       "7": ["11 PM"],
#>       "8": ["12 AM"],
#>       "9": ["1 AM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_005__02": {
#>     "Description": ["Free Days: I wake up at: Minutes"],
#>     "Levels": {
#>       "1": ["0 minutes"],
#>       "10": ["45 minutes"],
#>       "11": ["50 minutes"],
#>       "12": ["55 minutes"],
#>       "2": ["5 minutes"],
#>       "3": ["10 minutes"],
#>       "4": ["15 minutes"],
#>       "5": ["20 minutes"],
#>       "6": ["25 minutes"],
#>       "7": ["30 minutes"],
#>       "8": ["35 minutes"],
#>       "9": ["40 minutes"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_006": {
#>     "Description": ["Free Days: After _________ minutes I get up."],
#>     "Levels": {
#>       "0": ["0"],
#>       "9": ["9"],
#>       "10": ["10"],
#>       "11": ["15"],
#>       "12": ["20"],
#>       "13": ["25"],
#>       "14": ["30"],
#>       "15": ["40"],
#>       "16": ["50"],
#>       "17": ["1 hour"],
#>       "18": ["1 hour 15 minute"],
#>       "1": ["1"],
#>       "19": ["1 hour 30 minute"],
#>       "20": ["1 hour 45 minute"],
#>       "21": ["2 hours"],
#>       "22": ["3 hours"],
#>       "23": ["4 hours"],
#>       "2": ["2"],
#>       "3": ["3"],
#>       "4": ["4"],
#>       "5": ["5"],
#>       "6": ["6"],
#>       "7": ["7"],
#>       "8": ["8"]
#>     },
#>     "Units": ["minutes (min)"],
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_007": {
#>     "Description": ["On free days, I wake up by using an alarm clock or my parents wake me up."],
#>     "Levels": {
#>       "1": ["Yes"],
#>       "0": ["No"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008": {
#>     "Description": ["On free days, are you able to freely choose your bed time and wake-up time?"],
#>     "Levels": {
#>       "1": ["Yes"],
#>       "0": ["No"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01___1": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Family member/pet(s)"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01___2": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Work"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01___3": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Sports"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01___4": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Hobbies"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01___5": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Other"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v1___1": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Family member/pet(s) [Version 1]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v1___2": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Work [Version 1]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v1___3": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Sports [Version 1]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v1___4": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Hobbies [Version 1]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v1___5": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Other [Version 1]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v2___1": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Family member/pet(s) [Version 2]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v2___2": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Work [Version 2]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v2___3": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Sports [Version 2]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v2___4": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Hobbies [Version 2]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v2___5": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Church/religious services or activities [Version 2]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v2___6": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Chores [Version 2]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v2___7": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: House rules [Version 2]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v2___8": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: School/classes [Version 2]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v2___9": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Activities with family or friends [Version 2]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd_008__01__v2___10": {
#>     "Description": ["Freely choose your bed time and wake-up time: If no, what are the reasons? [Multi-select]: Activity not listed [Version 2]"],
#>     "Levels": {
#>       "1": ["TRUE"],
#>       "0": ["FALSE"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__school_001": {
#>     "Description": ["Do you go to school on a regular basis?"],
#>     "Levels": {
#>       "1": ["Yes"],
#>       "0": ["No"],
#>       "777": ["Decline to answer"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__school_001__01": {
#>     "Description": ["I go to school on _____ day(s) per week."],
#>     "Levels": {
#>       "1": ["1"],
#>       "2": ["2"],
#>       "3": ["3"],
#>       "4": ["4"],
#>       "5": ["5"],
#>       "6": ["6"],
#>       "7": ["7"]
#>     },
#>     "Units": ["days (d)"],
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__school_002": {
#>     "Description": ["My usual school schedule starts at: What part of the day?"],
#>     "Levels": {
#>       "1": ["Morning (4:00 AM to 11:59 AM)"],
#>       "2": ["Afternoon (12:00 PM Noon to 4:59 PM)"],
#>       "3": ["Evening (5:00 PM to 8:59 PM)"],
#>       "4": ["Nighttime (9:00 PM to 3:59 AM)"],
#>       "0": ["I don't regularly travel to school / I am home schooled"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__school_002__01a": {
#>     "Description": ["My usual school schedule starts at - 4am to 4pm: Hour"],
#>     "Levels": {
#>       "1": ["4 AM"],
#>       "10": ["1 PM"],
#>       "11": ["2 PM"],
#>       "12": ["3 PM"],
#>       "13": ["4 PM"],
#>       "2": ["5 AM"],
#>       "3": ["6 AM"],
#>       "4": ["7 AM"],
#>       "5": ["8 AM"],
#>       "6": ["9 AM"],
#>       "7": ["10 AM"],
#>       "8": ["11 AM"],
#>       "9": ["12 PM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__school_002__01b": {
#>     "Description": ["My usual school schedule starts at - 5pm to 3am: Hour"],
#>     "Levels": {
#>       "1": ["5 PM"],
#>       "10": ["2 AM"],
#>       "11": ["3 AM"],
#>       "2": ["6 PM"],
#>       "3": ["7 PM"],
#>       "4": ["8 PM"],
#>       "5": ["9 PM"],
#>       "6": ["10 PM"],
#>       "7": ["11 PM"],
#>       "8": ["12 AM"],
#>       "9": ["1 AM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__school_002__02": {
#>     "Description": ["My usual school schedule starts at: Minutes"],
#>     "Levels": {
#>       "1": ["0 minutes"],
#>       "10": ["45 minutes"],
#>       "11": ["50 minutes"],
#>       "12": ["55 minutes"],
#>       "2": ["5 minutes"],
#>       "3": ["10 minutes"],
#>       "4": ["15 minutes"],
#>       "5": ["20 minutes"],
#>       "6": ["25 minutes"],
#>       "7": ["30 minutes"],
#>       "8": ["35 minutes"],
#>       "9": ["40 minutes"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__school_003": {
#>     "Description": ["I leave the house to go to school in the morning at: What part of the day?"],
#>     "Levels": {
#>       "1": ["Morning (4:00 AM to 11:59 AM)"],
#>       "2": ["Afternoon (12:00 PM Noon to 4:59 PM)"],
#>       "3": ["Evening (5:00 PM to 8:59 PM)"],
#>       "4": ["Nighttime (9:00 PM to 3:59 AM)"],
#>       "0": ["I don't regularly travel to school / I am home schooled"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__school_003__01a": {
#>     "Description": ["I leave the house to go to school in the morning at - 4am to 4pm: Hour"],
#>     "Levels": {
#>       "1": ["4 AM"],
#>       "10": ["1 PM"],
#>       "11": ["2 PM"],
#>       "12": ["3 PM"],
#>       "13": ["4 PM"],
#>       "2": ["5 AM"],
#>       "3": ["6 AM"],
#>       "4": ["7 AM"],
#>       "5": ["8 AM"],
#>       "6": ["9 AM"],
#>       "7": ["10 AM"],
#>       "8": ["11 AM"],
#>       "9": ["12 PM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__school_003__01b": {
#>     "Description": ["I leave the house to go to school in the morning at - 5pm to 3am: Hour"],
#>     "Levels": {
#>       "1": ["5 PM"],
#>       "10": ["2 AM"],
#>       "11": ["3 AM"],
#>       "2": ["6 PM"],
#>       "3": ["7 PM"],
#>       "4": ["8 PM"],
#>       "5": ["9 PM"],
#>       "6": ["10 PM"],
#>       "7": ["11 PM"],
#>       "8": ["12 AM"],
#>       "9": ["1 AM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__school_003__02": {
#>     "Description": ["I leave the house to go to school in the morning at: Minutes"],
#>     "Levels": {
#>       "1": ["0 minutes"],
#>       "10": ["45 minutes"],
#>       "11": ["50 minutes"],
#>       "12": ["55 minutes"],
#>       "2": ["5 minutes"],
#>       "3": ["10 minutes"],
#>       "4": ["15 minutes"],
#>       "5": ["20 minutes"],
#>       "6": ["25 minutes"],
#>       "7": ["30 minutes"],
#>       "8": ["35 minutes"],
#>       "9": ["40 minutes"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_001": {
#>     "Description": ["School Days: I go to bed at: What part of the day?"],
#>     "Levels": {
#>       "1": ["Morning (4:00 AM to 11:59 AM)"],
#>       "2": ["Afternoon (12:00 PM Noon to 4:59 PM)"],
#>       "3": ["Evening (5:00 PM to 8:59 PM)"],
#>       "4": ["Nighttime (9:00 PM to 3:59 AM)"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_001__01a": {
#>     "Description": ["School Days: I go to bed at - 4am to 4pm: Hour"],
#>     "Levels": {
#>       "1": ["4 AM"],
#>       "10": ["1 PM"],
#>       "11": ["2 PM"],
#>       "12": ["3 PM"],
#>       "13": ["4 PM"],
#>       "2": ["5 AM"],
#>       "3": ["6 AM"],
#>       "4": ["7 AM"],
#>       "5": ["8 AM"],
#>       "6": ["9 AM"],
#>       "7": ["10 AM"],
#>       "8": ["11 AM"],
#>       "9": ["12 PM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_001__01b": {
#>     "Description": ["School Days: I go to bed at - 5pm to 3am: Hour"],
#>     "Levels": {
#>       "1": ["5 PM"],
#>       "10": ["2 AM"],
#>       "11": ["3 AM"],
#>       "2": ["6 PM"],
#>       "3": ["7 PM"],
#>       "4": ["8 PM"],
#>       "5": ["9 PM"],
#>       "6": ["10 PM"],
#>       "7": ["11 PM"],
#>       "8": ["12 AM"],
#>       "9": ["1 AM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_001__02": {
#>     "Description": ["School Days: I go to bed at: Minutes"],
#>     "Levels": {
#>       "1": ["0 minutes"],
#>       "10": ["45 minutes"],
#>       "11": ["50 minutes"],
#>       "12": ["55 minutes"],
#>       "2": ["5 minutes"],
#>       "3": ["10 minutes"],
#>       "4": ["15 minutes"],
#>       "5": ["20 minutes"],
#>       "6": ["25 minutes"],
#>       "7": ["30 minutes"],
#>       "8": ["35 minutes"],
#>       "9": ["40 minutes"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_002": {
#>     "Description": ["School Days: I actually start trying to fall asleep at: What part of the day?"],
#>     "Levels": {
#>       "1": ["Morning (4:00 AM to 11:59 AM)"],
#>       "2": ["Afternoon (12:00 PM Noon to 4:59 PM)"],
#>       "3": ["Evening (5:00 PM to 8:59 PM)"],
#>       "4": ["Nighttime (9:00 PM to 3:59 AM)"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_002__01a": {
#>     "Description": ["School Days: I actually start trying to fall asleep at - 4am to 4pm: Hour"],
#>     "Levels": {
#>       "1": ["4 AM"],
#>       "10": ["1 PM"],
#>       "11": ["2 PM"],
#>       "12": ["3 PM"],
#>       "13": ["4 PM"],
#>       "2": ["5 AM"],
#>       "3": ["6 AM"],
#>       "4": ["7 AM"],
#>       "5": ["8 AM"],
#>       "6": ["9 AM"],
#>       "7": ["10 AM"],
#>       "8": ["11 AM"],
#>       "9": ["12 PM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_002__01b": {
#>     "Description": ["School Days: I actually start trying to fall asleep at - 5pm to 3am: Hour"],
#>     "Levels": {
#>       "1": ["5 PM"],
#>       "10": ["2 AM"],
#>       "11": ["3 AM"],
#>       "2": ["6 PM"],
#>       "3": ["7 PM"],
#>       "4": ["8 PM"],
#>       "5": ["9 PM"],
#>       "6": ["10 PM"],
#>       "7": ["11 PM"],
#>       "8": ["12 AM"],
#>       "9": ["1 AM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_002__02": {
#>     "Description": ["School Days: I actually start trying to fall asleep at: Minutes"],
#>     "Levels": {
#>       "1": ["0 minutes"],
#>       "10": ["45 minutes"],
#>       "11": ["50 minutes"],
#>       "12": ["55 minutes"],
#>       "2": ["5 minutes"],
#>       "3": ["10 minutes"],
#>       "4": ["15 minutes"],
#>       "5": ["20 minutes"],
#>       "6": ["25 minutes"],
#>       "7": ["30 minutes"],
#>       "8": ["35 minutes"],
#>       "9": ["40 minutes"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_003": {
#>     "Description": ["School Days: I need ________ minutes to fall asleep."],
#>     "Levels": {
#>       "0": ["0"],
#>       "9": ["9"],
#>       "10": ["10"],
#>       "11": ["15"],
#>       "12": ["20"],
#>       "13": ["25"],
#>       "14": ["30"],
#>       "15": ["40"],
#>       "16": ["50"],
#>       "17": ["1 hour"],
#>       "18": ["1 hour 15 minute"],
#>       "1": ["1"],
#>       "19": ["1 hour 30 minute"],
#>       "20": ["1 hour 45 minute"],
#>       "21": ["2 hours"],
#>       "22": ["3 hours"],
#>       "23": ["4 hours"],
#>       "2": ["2"],
#>       "3": ["3"],
#>       "4": ["4"],
#>       "5": ["5"],
#>       "6": ["6"],
#>       "7": ["7"],
#>       "8": ["8"]
#>     },
#>     "Units": ["minutes (min)"],
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_004": {
#>     "Description": ["School Days: After falling asleep, I wake up ____ times during the night."],
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_004__01": {
#>     "Description": ["School Days: Altogether, these awakenings last ______minutes."],
#>     "Levels": {
#>       "0": ["0"],
#>       "9": ["9"],
#>       "10": ["10"],
#>       "11": ["15"],
#>       "12": ["20"],
#>       "13": ["25"],
#>       "14": ["30"],
#>       "15": ["40"],
#>       "16": ["50"],
#>       "17": ["1 hour"],
#>       "18": ["1 hour 15 minute"],
#>       "1": ["1"],
#>       "19": ["1 hour 30 minute"],
#>       "20": ["1 hour 45 minute"],
#>       "21": ["2 hours"],
#>       "22": ["3 hours"],
#>       "23": ["4 hours"],
#>       "2": ["2"],
#>       "3": ["3"],
#>       "4": ["4"],
#>       "5": ["5"],
#>       "6": ["6"],
#>       "7": ["7"],
#>       "8": ["8"]
#>     },
#>     "Units": ["minutes (min)"],
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_005": {
#>     "Description": ["School Days: I wake up at: What part of the day?"],
#>     "Levels": {
#>       "1": ["Morning (4:00 AM to 11:59 AM)"],
#>       "2": ["Afternoon (12:00 PM Noon to 4:59 PM)"],
#>       "3": ["Evening (5:00 PM to 8:59 PM)"],
#>       "4": ["Nighttime (9:00 PM to 3:59 AM)"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_005__01a": {
#>     "Description": ["School Days: I wake up at - 4am to 4pm: Hour"],
#>     "Levels": {
#>       "1": ["4 AM"],
#>       "10": ["1 PM"],
#>       "11": ["2 PM"],
#>       "12": ["3 PM"],
#>       "13": ["4 PM"],
#>       "2": ["5 AM"],
#>       "3": ["6 AM"],
#>       "4": ["7 AM"],
#>       "5": ["8 AM"],
#>       "6": ["9 AM"],
#>       "7": ["10 AM"],
#>       "8": ["11 AM"],
#>       "9": ["12 PM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_005__01b": {
#>     "Description": ["School Days: I wake up at - 5pm to 3am: Hour"],
#>     "Levels": {
#>       "1": ["5 PM"],
#>       "10": ["2 AM"],
#>       "11": ["3 AM"],
#>       "2": ["6 PM"],
#>       "3": ["7 PM"],
#>       "4": ["8 PM"],
#>       "5": ["9 PM"],
#>       "6": ["10 PM"],
#>       "7": ["11 PM"],
#>       "8": ["12 AM"],
#>       "9": ["1 AM"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_005__02": {
#>     "Description": ["School Days: I wake up at: Minutes"],
#>     "Levels": {
#>       "1": ["0 minutes"],
#>       "10": ["45 minutes"],
#>       "11": ["50 minutes"],
#>       "12": ["55 minutes"],
#>       "2": ["5 minutes"],
#>       "3": ["10 minutes"],
#>       "4": ["15 minutes"],
#>       "5": ["20 minutes"],
#>       "6": ["25 minutes"],
#>       "7": ["30 minutes"],
#>       "8": ["35 minutes"],
#>       "9": ["40 minutes"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_006": {
#>     "Description": ["School Days: After _________ minutes I get up."],
#>     "Levels": {
#>       "0": ["0"],
#>       "9": ["9"],
#>       "10": ["10"],
#>       "11": ["15"],
#>       "12": ["20"],
#>       "13": ["25"],
#>       "14": ["30"],
#>       "15": ["40"],
#>       "16": ["50"],
#>       "17": ["1 hour"],
#>       "18": ["1 hour 15 minute"],
#>       "1": ["1"],
#>       "19": ["1 hour 30 minute"],
#>       "20": ["1 hour 45 minute"],
#>       "21": ["2 hours"],
#>       "22": ["3 hours"],
#>       "23": ["4 hours"],
#>       "2": ["2"],
#>       "3": ["3"],
#>       "4": ["4"],
#>       "5": ["5"],
#>       "6": ["6"],
#>       "7": ["7"],
#>       "8": ["8"]
#>     },
#>     "Units": ["minutes (min)"],
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_007": {
#>     "Description": ["School Days: On school days, I wake up by using an alarm clock or my parents wake me up"],
#>     "Levels": {
#>       "1": ["Yes"],
#>       "0": ["No"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__sd_007__01": {
#>     "Description": ["School Days: I regularly wake up BEForE the alarm rings:"],
#>     "Levels": {
#>       "1": ["Yes"],
#>       "0": ["No"]
#>     },
#>     "Derivative": [false]
#>   },
#>   "ph_y_mctq__fd__bed__end__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - In bed end): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__bed__end__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - In bed end): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__bed__start__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - In bed start): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__bed__start__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - In bed start): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__bed_sum": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - In bed): Sum"],
#>     "Units": ["hours (h)"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep__end__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep end): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep__end__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep end): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep__mid__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep mid): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep__mid__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep mid): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep__onset__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep onset): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep__onset__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep onset): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep__start__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep start): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep__start__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep start): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep__waso_sum": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep wakenings after sleep onset): Sum"],
#>     "Units": ["hours (h)"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep_dur": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep): Duration"],
#>     "Units": ["hours (h)"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep_inertia": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep): Inertia"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep_latent": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep): Latency"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd__sleep_period": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day - Sleep): Period"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__fd_count": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Free Day): Count"],
#>     "Units": ["days (d)"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__raw__36h_chrono": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Raw: Chronotype): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__school__leave__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] ( School Schedule leave): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__school__leave__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] ( School Schedule leave): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__school__start__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] ( School Schedule start): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__school__start__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] ( School Schedule start): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__bed__end__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - In bed end): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__bed__end__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - In bed end): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__bed__start__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - In bed start): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__bed__start__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - In bed start): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__bed_sum": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - In bed): Sum"],
#>     "Units": ["hours (h)"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep__end__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep end): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep__end__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep end): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep__mid__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep mid): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep__mid__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep mid): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep__onset__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep onset): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep__onset__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep onset): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep__start__24h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep start): Time [24 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep__start__36h_t": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep start): Time [36 hour adjusted]"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep__waso_sum": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep wakenings after sleep onset): Sum"],
#>     "Units": ["hours (h)"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep_dur": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep): Duration"],
#>     "Units": ["hours (h)"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep_inertia": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep): Inertia"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep_latent": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep): Latency"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd__sleep_period": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day - Sleep): Period"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sd_count": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (School Day): Count"],
#>     "Units": ["days (d)"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sleep_dur": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Sleep): Duration"],
#>     "Units": ["hours (h)"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sleep_loss": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Sleep): Loss"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__sleep_period": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Sleep): Period"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__socjl_absl": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Social Jetlag: Absolute): Time"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq__socjl_rel": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Social Jetlag: Relative): Time"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq_chrono": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth] (Chronotype): Time"],
#>     "Derivative": [true]
#>   },
#>   "ph_y_mctq_outlier": {
#>     "Description": ["Munich Chronotype Questionnaire [Youth]: Outlier"],
#>     "Levels": {
#>       "0": ["No"],
#>       "1": ["Yes"]
#>     },
#>     "Derivative": [true]
#>   }
#> } 
```
