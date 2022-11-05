This README file was generated on 2022-07-12 by Nicolas Gomez Andujar

GENERAL INFORMATION

1. Title of Dataset:  Social-Cohesion-Small-scale-Fisheries-Puerto-Rico

2. Dataset Summary: Repository for the anonimized raw data, processed data, and code used in the quantitative social network analysis of cooperation networks in the commercial small-scale fisheries of Puerto Rico during 2020. This repository encompasses the quantiative data utilized for the peer-reviewed article published in the Frontiers in Marine Science journal (https://doi.org/10.3389/fmars.2022.966309), and represents a sub-set of the datasets obtained throughout this research project stored in the PR-SSF-SNA Github repository. (Link to repository: https://github.com/NicoGomezAndujar/PR-SSF-SNA). 

3. Author Information
	A. Principal Investigator Contact Information
		Name: Nicolas X. Gomez Andujar (NXGA)
		Institution: Oregon State University
		Address: PO Box 637 Culebra, PR 0075
		Email: gomezann@oregonstate.edu ; nicolas.x.gomez@gmail.com 

	B. Co-investigator Contact Information
		Name: Dr. James Watson (JW)
		Institution: Oregon State University
		Address: Strand Agriculture Hall 348B
                         170 SW Waldo Place
                         Corvallis, OR 97331
		Email: james.watson@oregonstate.edu

	C. Co-investigator Contact Information
		Name: Dr. Andrew Gerkey (AW)
		Institution: Oregon State University
		Address: Waldo Hall 218
                        2250 SW Jefferson Way
                        Corvallis, OR 97331
		Email: drew.gerkey@oregonstate.edu

4. Date of data collection:  2020-08-21 to 2021-01-01


5. Geographic location of data collection:
                Places where fishers drop off their catch (landing site) inside the following municipalities of north-eastern Puerto Rico: 
                1) Culebra Municipality: Landing site of Culebra (Lat: 18.301327 Long: -65.300414)  
                2) Naguabo Municipality: Landing site of Hucares (Lat: 18.186852 Long: -65.710920) 
                3) Fajardo Municipality: Landing site of Maternillo (Lat: 18.332264 Long:-65.627925) 
                                         Landing site of Las Croabas (Lat: 18.364555, Long: -65.625614)
                                         Landing site of Sardinera (Lat: 18.346303, Long:  -65.636471)

6. Information about funding sources that supported the collection of the data: 
                NXGA was supported by the Oregon State University Pipeline Diversity Fellowship via tuition and living stipend during data collection and analysis.                     Apart from this, no other funding was used for to collect of analye this data. 


SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data:  Creative Commons Attribution License (CC BY). 

2. Links to publications that cite or use the data: https://www.frontiersin.org/articles/10.3389/fmars.2022.966309/full

3. Links/relationships to ancillary data sets: 

4. Was data derived from another source? No

6. Recommended citation for this dataset: 

Gomez-Andujar N., Watson J., Gerkey A., Conway F., (2022). Social cohesion and self-governance arrangements among small-scale fisheries in Puerto Rico. Frontiers in Marine Science. https://www.frontiersin.org/articles/10.3389/fmars.2022.966309/full


DATA & FILE OVERVIEW

1. File List: 
<list all files (or folders, as appropriate for dataset organization) contained in the dataset, with a brief description>

2. Relationship between files, if important: Use in numeric order to carry out the analysis presented in the publication.

3. Additional related data collected that was not included in the current data package: 
   1. Gear and sectorial conflicts, in the form of ego-centric network data. 
   2. Network of work-support among commercial fishers, not just institutional actors. 
   3. Attributes of fishers related to social capital and markets during the pandemic.

4. Are there multiple versions of the dataset? No


METHODOLOGICAL INFORMATION

1. Description of methods used for collection/generation of data: 
<Include links or references to publications or other documentation containing experimental design or protocols used in data collection>

2. Methods for processing the data: 
<describe how the submitted data were generated from the raw or collected data>

3. Instrument- or software-specific information needed to interpret the data: e. All quantitative analysis was done in R software, version 3.6.1. Network processing
321 and exploratory analysis were done in the network, sna, igraph, and GGally packages (Butts, 2007 Butts, 2008; Csardi & Nepusz, 2006; Schloerke et al., 2018). The statnet and ergm packages were used for the ERGM modeling (Handcock et al., 2008; Hunter et al., 2008). 


4. People involved with sample collection, processing, analysis and/or submission: Only Nicolas Gomez 


DATA-SPECIFIC INFORMATION FOR: [FILENAME]
<repeat this section for each dataset, folder or file, as appropriate>

1. Number of variables: 

2. Number of cases/rows: 

3. Variable List: 
<list variable name(s), description(s), unit(s)and value labels as appropriate for each>

4. Missing data codes: 
<list code/symbol and definition>n

5. Specialized formats or other abbreviations used: 
SSF = small-scale fisheries
PR= Puerto Rico
Ws = work-support
sna = network object
ERGM= exponential random graph model
