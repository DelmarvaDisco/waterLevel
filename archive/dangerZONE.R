#Connect
db <- dbConnect(PostgreSQL(), 
                host     = "sesync-postgis01.research.sesync.org",
                dbname   = "choptank",
                user     = "palmergroup", 
                password = 'hor9rnds')

#Burn it down
dbGetQuery(db, "delete FROM odm2.actions 
                WHERE methodID= (SELECT methodID FROM odm2.methods 
                                 WHERE methodcode = 'PT_Download')")

#Select adn delete specefic date  
dbGetQuery(db, "delete FROM odm2.results 
                   WHERE ResultID= (SELECT ResultID FROM odm2.results 
                                         WHERE SamplingFeatureCode = 'DK Wetland Well Shallow')")

