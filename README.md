## 1. Overview  

This repository contains the replication materials for the article "A Framework for Studying Causal Effects of Speech Style: Application to U.S. Presidential Campaigns," *Journal of the Royal Statistical Society: Series A*.

Note that roughly 6 gigabytes of large media files are represented as Git Large File Storage (LFS) pointers. To fully clone this repository, please [install Git LFS](https://docs.github.com/en/repositories/working-with-files/managing-large-files/installing-git-large-file-storage), run the usual `git clone` to obtain code and LFS pointers, and finally `git lfs pull` to obtain media files.

## 2. Contents  

The repository is organized as follows. 

- **`README.md`**: This file, providing an overview of the replication archive.  
- **`code/`**: Contains R scripts used for data processing and analysis.  
  - `script0_transcribe_wavs.R`: Processes speech recordings and generates transcripts.  
  - `script1_analyze_campaign_corpus.R`: Analyzes the campaign speech corpus.  
  - `script2_clean_and_merge_naturalistic_experiment.R`: Cleans and merges data for naturalistic experiment.  
  - `script3_analyze_naturalistic_experiment.R`: Analyzes naturalistic experiment.  
  - `script4_analyze_actor_experiment.R`: Analyzes the actor-voiced experiment.  
  - `script5_actor_audio_balance_checks.R`: Performs balance checks on audio features.  
- **`data/`**: Raw data.
- **`figures/`**: Stores output figures generated from the scripts.  
- **`intermediate_files/`**: Stores intermediate datasets and results.  

---
