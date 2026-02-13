
# Define what bears will define speed. Bears were identified via a random generator choosing 5 for each sex.
# This is used for outlier calculations
Bear1 <- readr::read_csv("Derived Data/BFB/2021_BFB_Positions.csv")
Bear1 <- as.data.frame(Bear1)
Bear1tele <- as.telemetry(Bear1, projection = "EPSG:6514")
Bear1Guess <- ctmm.guess(Bear1tele, interactive = FALSE)
Bear1fit <- ctmm.fit(Bear1tele, CTMM = Bear1Guess, method = "pHREML")
Bear1speed <- ctmm::speed(Bear1tele, CTMM = Bear1fit, level = 0.95)

Bear2<- readr::read_csv("Derived Data/Collar_ZZ/2021_ZZ_Positions.csv")
Bear2 <- as.data.frame(Bear2) 
Bear2tele <- as.telemetry(Bear2, projection = "EPSG:6514")
Bear2Guess<- ctmm.guess(Bear2tele, interactive = FALSE)
Bear2fit <- ctmm.fit(Bear2tele, CTMM = Bear2Guess, method = "pHREML")
Bear2speed <- ctmm::speed(Bear2tele, CTMM = Bear2fit, level = 0.95)

Bear3 <- readr::read_csv("Derived Data/Mop/2023_Mop_Positions.csv")
Bear3 <- as.data.frame(Bear3)
Bear3tele <- as.telemetry(Bear3, projection = "EPSG:6514")
Bear3Guess <- ctmm.guess(Bear3tele, interactive = FALSE)
Bear3fit <- ctmm.fit(Bear3tele, CTMM = Bear3Guess, method = "pHREML")
Bear3speed<- ctmm::speed(Bear3tele, CTMM = Bear3fit, level = 0.95)


Bear4 <- readr::read_csv("Derived Data/Ombre/2021_Ombre_Positions.csv")
Bear4<- as.data.frame(Bear4)
Bear4tele <- as.telemetry(Bear4, projection = "EPSG:6514")
Bear4Guess <- ctmm.guess(Bear4tele, interactive = FALSE)
Bear4fit <- ctmm.fit(Bear4$tele, CTMM = Bear4$Guess, method = "pHREML")
Bear4speed <- ctmm::speed(Bear4$tele,CTMM = Bear4fit, level = 0.95)


Bear5<- readr::read_csv("Derived Data/Squirt/2023_Squirt_Positions.csv")
Bear5<- as.data.frame(Bear5)
Bear5$tele<- as.telemetry(Bear5, projection = "EPSG:6514")
Bear5$Guess <- ctmm.guess(Bear5tele, interactive = FALSE)
Bear5$fit <- ctmm.fit(Bear5tele, CTMM = Bear5Guess, method = "pHREML")
Bear5$speed <- ctmm::speed(Bear5tele, CTMM = Bear5fit, level = 0.95)


Bear6 <- readr::read_csv("Derived Data/Troublemaker/2021_Trouble_Positions.csv")
Bear6<- as.data.frame(Bear6)
Bear6tele <- as.telemetry(Bear6, projection = "EPSG:6514")
Bear6Guess <- ctmm.guess(Bear6tele, interactive = FALSE)
Bear6fit <- ctmm.fit(Bear6tele, CTMM= Bear6Guess, method = "pHREML")
Bear6speed <- ctmm::speed(Bear6tele, CTMM = Bear6fit, level = 0.95)

Bear7 <- readr::read_csv("Derived Data/Fanta/2023_Fanta_Positions.csv")
Bear7 <- as.data.frame(Bear7)
Bear7tele <- as.telemetry(Bear7, projection = "EPSG:6514")
Bear7Guess <- ctmm.guess(Bear7tele, interactive = FALSE)
Bear7fit <- ctmm.fit(Bear7tele, CTMM = Bear7Guess, method = "pHREML")
Bear7speed <- ctmm::speed(Bear7tele, CTMM = Bear7fit, level = 0.95)

Bear8 <- readr::read_csv("Derived Data/Rattlesnake/2021_Rattlesnake_Positions.csv")
Bear8 <- as.data.frame(Bear8)
Bear8tele <- as.telemetry(Bear8, projection = "EPSG:6514")
Bear8Guess <- ctmm.guess(Bear8tele, interactive = FALSE)
Bear8fit <- ctmm.fit(Bear8tele, CTMM = Bear8Guess, method = "pHREML")
Bear8speed <- ctmm::speed(Bear8tele, CTMM = Bear8fit, level = 0.95)

Bear9<- readr::read_csv("Derived Data/Dahlia/2022_Dahlia_Positions.csv")
Bear9<- as.data.frame(Bear9)
Bear9tele <- as.telemetry(Bear9, projection = "EPSG:6514")
Bear9Guess <- ctmm.guess(Bear9tele, interactive = FALSE)
Bear9fit <- ctmm.fit(Bear9tele, CTMM = Bear9Guess, method = "pHREML")
Bear9speed <- ctmm::speed(Bear9tele, CTMM = Bear9fit, level = 0.95)

Bear10 <- readr::read_csv("Derived Data/Maple/2022_Maple_Positions.csv")
Bear10 <- as.data.frame(Bear10)
Bear10tele <- as.telemetry(Bear10, projection = "EPSG:6514")
Bear10Guess <- ctmm.guess(Bear10tele, interactive = FALSE)
Bear10fit <- ctmm.fit(Bear10tele, CTMM = Bear10Guess, method = "pHREML")
Bear10speed <- ctmm::speed(Bear10tele, CTMM = Bear10fit, level = 0.95)