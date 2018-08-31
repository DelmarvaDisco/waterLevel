dbGetQuery(db, "delete FROM odm2.actions WHERE methodID= (SELECT methodID FROM odm2.methods WHERE methodcode = 'PT_Download')")
