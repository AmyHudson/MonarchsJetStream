# pulling NPN data for Amy H's monarch-jet stream analysis
# TCrimmins 12-22-20
# Bounding box: 20N to 40N; -104E to -90E

library(rnpn)

# download individual-level phenometrics
# all flowering data
# subset to bounding box + 2010-2020

NPNdata <- npn_download_individual_phenometrics(request_source = "TCrimmins", years = c(2010:2020),
                                     coords = c(20, -104, 40, -90))
                                                                   
# subset to flowering
unique(NPNdata$phenophase_description)

NNflowering <- subset(NPNdata, phenophase_description=="Open flowers (1 location)" | phenophase_description=="Open flowers" |
                        phenophase_description=="Open flowers (lilac)")


#write out file so I can plot in excel (sorry!!)
write.csv(NNflowering,"c:/NPN/Manuscripts/Working/Hudson_etal_monarch_stuff/NNflowering.csv")