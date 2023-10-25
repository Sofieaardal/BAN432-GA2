# BAN432-GA2

# Political Landscape Analysis: Topic Modelling

In light of the increasing polarization of the political landscape, our goal is to assess what topics are strongly associated with either of the major US parties (Democrat vs Republican) using speeches by U.S. House representatives from the years 1995, 1996, 2021, and 2022.

## Task 1: Estimate a Topic Model

**Context**: 
Use the data to estimate a topic model, ensuring that most topics identified by the model have clear human interpretation. 

### Sub-Tasks:

- [ ] **Data Cleaning**
  - [ ] Import the data.
  - [ ] Convert to lower case.
  - [ ] Remove punctuations, numbers, and stop words.
  - [ ] Tokenize the data.
  - [ ] Decide on word limits.
  
- [ ] **Model Estimation**
  - [ ] Decide on the number of topics.
  - [ ] Train the topic model.
  - [ ] Display top 10 terms per topic.

## Task 2: Detailed Analysis 

### Sub-Tasks:

- [ ] **Dimensions of Document-Term-Matrix**: Extract dimensions from the DTM.
  
- [ ] **Number of topics**: Report as set during the LDA estimation.
  
- [ ] **Prevalent Topics Analysis**: 
  - [ ] Identify prevalent topics in the early sample (1995 & 1996).
  - [ ] Identify prevalent topics in the late sample (2021 & 2022).

- [ ] **Party Association Analysis**:
  - [ ] Determine topics mostly associated with the Democrats.
  - [ ] Determine topics mostly associated with the Republicans.

### Bonus:

- [ ] **Party-Topic Shift Analysis**: 
  - Identify topics associated predominantly with one party in the early sample but switched to the opposite party in the late sample.
