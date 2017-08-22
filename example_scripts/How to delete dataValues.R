#  get the dataset with all datavalues
dv <- getDHIS2_dataSet('Nursing Huddle', 'Haiti (Zamni Lasante)',"2016-01-01", "2017-02-27", usr, pwd, T, url)

# we need the default catOptionCombo id
catOpts <- getDHIS2_Resource('categoryOptionCombos', usr, pwd, url)

id <- catOpts$id[grep('default', catOpts$displayName)]

sub_dv <- dv[dv$attributeOptionCombo == id,]

resp <- deleteDHIS2_Values(sub_dv,750, usr, pwd, url)
