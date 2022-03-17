This repository is linked to the [working paper](https://doi.org/10.26092/elib/1443) entitled "The Development of Al in Multinational Enterprises - Effects upon Technological Trajectories and Innovation Performance", which can be found at https://doi.org/10.26092/elib/1443.

The paper abstract is:
*This paper investigates how the development of AI-related inventions by Multinational Enterprises (MNEs) affects their technological trajectories and innovative performance. I combine a matched-pair analysis with an extension of the Difference-in-Difference method to analyse these effects over a novel panel dataset of MNEs. This dataset links over 30 thousand MNEs to more than 10 million patents that these companies owned directly or indirectly (i.e., through their subsidiaries) in the period from 2011 to 2019. The results indicate that MNEs introducing AI-related inventions increase the relatedness of subsequent inventions by about 10 per cent compared to a control group. These results are robust when accounting for a self-selection bias. AI is thus being used to reinforce the existing technological trajectories, rather than to disrupt them. The results also suggest that the number of subsequent inventions is about 40 per cent higher for MNEs that introduce AI during the observation period compared to the control group, without significant effects on the intensity of R&D expenditures per invention. It is argued that this increase in innovative performance is linked not only to knowledge dynamics created by learning about AI but also by AI’s technical potential to be used for learning.*

This repository contains an R code and all data necessary to estimate all the effects of AI adoption presented in the paper. 

The data used to make these estimations comes from Bureau van Dijk's (Bvd) intellectual proprerty via Orbis and Orbis IP. For that reason, the data presented here is a simplified version in which key variables are ommited, i.e., BvD_Ids of companies, which is BvDs propritary identified. It uses an id generated to replace this BvD_Id information, and also omits the name of the companies. The IDs generated come from the result of the matching procedure in the paper (see section "3.2. Matching AI Adopters to Non-adopters"). In this matching procedure, some control companies can be combined more than once to treated companies, meaning that some distinct ids may refer to the same company matched more than 1 time (this happens for roughly less than 5% of the companies).

Two files from the Data folder[https://github.com/matheusleusin/AI_and_MNEs/tree/main/Data] are used to reproduce the code:
*Data_matched_MNEs.csv* - This data file contains information about the generated ids, the calculated relatedness, NACE sectors, and other MNE-specific indicators  used to make the estimations. The time range goes from 2006 to 2019. This data file looks like this:
![image](https://user-images.githubusercontent.com/58182885/158807937-76bb68f0-4778-423d-8cc1-c6a8fa9f3dff.png)
*SquareMatrixSectors.csv* - This data file presents a square matrix linking NACE sectors to the number of patents identified as registered by any company of these sectors for any 4-digits IPC code. The file looks like this:
![image](https://user-images.githubusercontent.com/58182885/158806643-2223ead0-b47b-46e8-898d-662cf28539d3.png)

Two additional files are generated by running the code: Distance_categories and Distance_categories2. They refer to respectively to an intermediary and to the final estimation of technological distance from distinct NACE sectors to the cluster of AI technologies.
