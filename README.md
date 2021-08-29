# GETMedKnowledge
This repository provides example R code using the RxNorm and openFDA APIs to retrieve the tall man synonym and boxed warning associated with a list of NDCs (the input NDCs must be formatted as 11-digit values)

The NDC output table returns boxed warning and tall man lettering info for each NDC in the input. If there was no tall man lettering associated with the NDC, the RxNorm name is returned 

The RXCUI output table groups the NDC output table by RXCUI and returns applicable boxed warning and tall man lettering info for each RXCUI. It also returns a count of how many NDCs were classified by the given RXCUI
