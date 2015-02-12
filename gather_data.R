##      Get Move Scores
# require: -----------------------------------
require(stringr)

# Varibles:
weak_1 <- -24
weak_2 <- -39
weak_3 <- -49
weak_4 <- -59
weak_5 <- -69
weak_6 <- -79
weak_7 <- -89
weak_8 <- -99
weak_9 <- -109
weak_10 <- -149

even_game <- 299

## read in new stockfish files ===============
# black stockfish file -----------------------
b_score <- read.csv("/Users/craigzimber/Documents/R/finding-elo/bScore.csv", 
                    header=TRUE, sep = ",", stringsAsFactors = FALSE) 
# white stockfish file -----------------------
w_score <- read.csv("/Users/craigzimber/Documents/R/finding-elo/wScore.csv", 
                    header=TRUE, sep = ",", stringsAsFactors = FALSE) 

## Create data frame to gather needed 
# Column Names of data frame -----------------
df_names <- c(1:48) 

# Create data frame: 50,000 rows, df_names ---
df <- data.frame(matrix(vector(), 
                        50000, length(df_names), 
                        dimnames=list(c(), df_names)), 
                 stringsAsFactors=F)

#j <- nrow(t.w.05)
for (k in 1:1000){
        # Extract "i" scores -------------------------
        # This starts a loop. replace "3" with "k" ---
        w <- w_score[k, ]
        b <- b_score[k, ]
        
        # Transpose row to column and cbind ----------
        w <- as.data.frame(t(w[, -1]))
        b <- as.data.frame(t(b[,-1]))
        z <- cbind(w, b)
        # as.numeric ---------------------------------
        z[, 1] <- as.character(z[, 1])
        z[, 2] <- as.character(z[, 2])
        z[, 1] <- as.numeric(z[, 1])
        z[, 2] <- as.numeric(z[, 2])
        
        ### Get delta for White scores "i" ===========
        # Get Previous Score for White "i" -----------
        # set column as NA ---------------------------
        z$w.prev.score <- NA
        
        # Move score down one row --------------------
        j <- nrow(z) - 1
        for (i in 2:j) {
                z[i, 3] <- z[(i - 1), 2]        
        }
        
        ## Keep only rows that have moves
        # Create new helper column [, 4] to hold results of 1 or 0 or 
        # NA if there are no moves
        # Subset to remove rows that have no moves (NA)
        z[, 4] <- ifelse(z[, 1] + z[, 1] > 0, 1, 0)
        z <- subset(z, z[, 4] >= 0)
        # Remove helper column
        z <- z[, -4]
        z
        # Get delta ----------------------------------
        z$w.delta   <- z[, 1] - z[, 3]
        head(z)        
        z
        ## Get White Weak Moves 1-9 ==================
        # Get ABS game score
        z[, 5] <- abs(z[, 1])
        # [, 5] is ABS game score
        # [, 4] is White's delta
        z$w.weak.move.1 <- ifelse(z[, 5] < even_game & z[, 4] < weak_1, 1, 0)      
        z$w.weak.move.2 <- ifelse(z[, 5] < even_game & z[, 4] < weak_2, 1, 0)      
        z$w.weak.move.3 <- ifelse(z[, 5] < even_game & z[, 4] < weak_3, 1, 0)      
        z$w.weak.move.4 <- ifelse(z[, 5] < even_game & z[, 4] < weak_4, 1, 0)      
        z$w.weak.move.5 <- ifelse(z[, 5] < even_game & z[, 4] < weak_5, 1, 0)      
        z$w.weak.move.6 <- ifelse(z[, 5] < even_game & z[, 4] < weak_6, 1, 0)      
        z$w.weak.move.7 <- ifelse(z[, 5] < even_game & z[, 4] < weak_7, 1, 0)      
        z$w.weak.move.8 <- ifelse(z[, 5] < even_game & z[, 4] < weak_8, 1, 0)      
        z$w.weak.move.9 <- ifelse(z[, 5] < even_game & z[, 4] < weak_9, 1, 0)      
        ##############################################
        ### Get delta for Black scores "i" ===========
        # Get Previous Score for Black "i" -----------
        ## set column as NA ---------------------------
        ## z$b.prev.score <- NA
        # Change sign of Black's Prev.Score ----------
        z$b.prev.score <- -z[, 1]
        # z[1, 15] <- NA
        # Create B.New.Score ------------------------
        # z$b.new.score <- NA
        # Change sign of Black's New.Score ----------
        z$b.new.score <- -z[, 2]
        #z[1, 16] <- NA
        # Get delta ---------------------------------
        z$b.delta <- z[, 16] - z[, 15]        
        ## Get Black Weak Moves 1-3 =================
        z$b.weak.move.1 <- ifelse(z[, 5] < even_game & z[, 17] < weak_1, 1, 0)      
        z$b.weak.move.2 <- ifelse(z[, 5] < even_game & z[, 17] < weak_2, 1, 0)      
        z$b.weak.move.3 <- ifelse(z[, 5] < even_game & z[, 17] < weak_3, 1, 0)      
        z$b.weak.move.4 <- ifelse(z[, 5] < even_game & z[, 17] < weak_4, 1, 0)      
        z$b.weak.move.5 <- ifelse(z[, 5] < even_game & z[, 17] < weak_5, 1, 0)      
        z$b.weak.move.6 <- ifelse(z[, 5] < even_game & z[, 17] < weak_6, 1, 0)      
        z$b.weak.move.7 <- ifelse(z[, 5] < even_game & z[, 17] < weak_7, 1, 0)      
        z$b.weak.move.8 <- ifelse(z[, 5] < even_game & z[, 17] < weak_8, 1, 0)      
        z$b.weak.move.9 <- ifelse(z[, 5] < even_game & z[, 17] < weak_9, 1, 0)      
        
        ########################
        ## Get delta Summary: 1st Qu., Median, 3rd Qu.
        # ALL_w_prev_score_median / mean / sd
        xx <- as.array(summary(z$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 1] <- xx[3, 2]
        df[k, 2] <- xx[4, 2]
        df[k, 3] <- sd(z$w.prev.score, na.rm = TRUE)
        
        head(df[, 7:8])
        summary(df)
        # ALL_w_delta_median / mean / sd
        xx <- as.array(summary(z$w.delta))
        xx <- as.data.frame(xx)
        df[k, 4] <- xx[3, 2]
        df[k, 5] <- xx[4, 2]
        df[k, 6] <- sd(z$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # ALL_w_weak_move_1_mean / sd
        xx <- as.array(summary(z$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 7] <- xx[4, 2]
        df[k, 8] <- sd(z$w.delta, na.rm = TRUE)
        
        # ALL_w_weak_move_2_mean / sd
        xx <- as.array(summary(z$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 9] <- xx[4, 2]
        df[k, 10] <- sd(z$w.weak.move.2, na.rm = TRUE )
        
        # ALL_w_weak_move_3_mean / sd
        xx <- as.array(summary(z$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 11] <- xx[4, 2]
        df[k, 12] <- sd(z$w.weak.move.3, na.rm = TRUE)
        
        # ALL_w_weak_move_4_mean / sd
        xx <- as.array(summary(z$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 13] <- xx[4, 2]
        df[k, 14] <- sd(z$w.weak.move.4, na.rm = TRUE)
        
        # ALL_w_weak_move_5_mean / sd
        xx <- as.array(summary(z$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 15] <- xx[4, 2]
        df[k, 16] <- sd(z$w.weak.move.5, na.rm = TRUE)
        
        # ALL_w_weak_move_6_mean / sd
        xx <- as.array(summary(z$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 17] <- xx[4, 2]
        df[k, 18] <- sd(z$w.weak.move.6, na.rm = TRUE)
        
        # ALL_w_weak_move_7_mean / sd
        xx <- as.array(summary(z$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 19] <- xx[4, 2]
        df[k, 20] <- sd(z$w.weak.move.7, na.rm = TRUE)
        
        # ALL_w_weak_move_8_mean / sd
        xx <- as.array(summary(z$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 21] <- xx[4, 2]
        df[k, 22] <- sd(z$w.weak.move.8, na.rm = TRUE)
        
        # ALL_w_weak_move_9_mean / sd
        xx <- as.array(summary(z$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 23] <- xx[4, 2]
        df[k, 24] <- sd(z$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # ALL_b_prev_score_median / mean / sd
        xx <- as.array(summary(z$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 25] <- xx[3, 2]
        df[k, 26] <- xx[4, 2]
        df[k, 27] <- sd(z$b.prev.score, na.rm = TRUE)
        
        # ALL_b_delta_mean / sd
        xx <- as.array(summary(z$b.delta))
        xx <- as.data.frame(xx)
        df[k, 28] <- xx[3, 2]
        df[k, 29] <- xx[4, 2]
        df[k, 30] <- sd(z$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # ALL_b_weak_move_1_mean / sd
        xx <- as.array(summary(z$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 31] <- xx[4, 2]
        df[k, 32] <- sd(z$b.weak.move.1, na.rm = TRUE)
        
        # ALL_b_weak_move_2_mean / sd
        xx <- as.array(summary(z$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 33] <- xx[4, 2]
        df[k, 34] <- sd(z$b.weak.move.2, na.rm = TRUE)
        
        # ALL_b_weak_move_3_mean / sd
        xx <- as.array(summary(z$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 35] <- xx[4, 2]
        df[k, 36] <- sd(z$b.weak.move.3, na.rm = TRUE)
        
        # ALL_b_weak_move_4_mean / sd
        xx <- as.array(summary(z$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 37] <- xx[4, 2]
        df[k, 38] <- sd(z$b.weak.move.4, na.rm = TRUE)
        
        # ALL_b_weak_move_5_mean / sd
        xx <- as.array(summary(z$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 39] <- xx[4, 2]
        df[k, 40] <- sd(z$b.weak.move.5, na.rm = TRUE)
        
        # ALL_b_weak_move_6_mean / sd
        xx <- as.array(summary(z$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 41] <- xx[4, 2]
        df[k, 42] <- sd(z$b.weak.move.6, na.rm = TRUE)
        
        # ALL_b_weak_move_7_mean / sd
        xx <- as.array(summary(z$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 43] <- xx[4, 2]
        df[k, 44] <- sd(z$b.weak.move.7, na.rm = TRUE)
        
        # ALL_b_weak_move_8_mean / sd
        xx <- as.array(summary(z$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 45] <- xx[4, 2]
        df[k, 46] <- sd(z$b.weak.move.8, na.rm = TRUE)
        
        # ALL_b_weak_move_9_mean / sd
        xx <- as.array(summary(z$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 47] <- xx[4, 2]
        df[k, 48] <- sd(z$b.weak.move.9, na.rm = TRUE)
        
        # ??? Do i need this: ??
        #data[complete.cases(data$Shoulders), ]
        # df[complete.cases(df[k, ])]        
        
        ###########################################
        # divide above into 1st quarter ###########
        ###########################################
        ## subset z - 1st Qtr of game moves         
        z25 <- as.integer(nrow(z) * 0.25)
        zz <- z[1:z25, ]
        
        ###################
        # ALL_1st_w_prev_score_median / mean / sd
        xx <- as.array(summary(zz$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 49] <- xx[3, 2]
        df[k, 50] <- xx[4, 2]
        df[k, 51] <- sd(zz$w.prev.score, na.rm = TRUE)
        
        # ALL_1st_w_delta_median / mean / sd
        xx <- as.array(summary(zz$w.delta))
        xx <- as.data.frame(xx)
        df[k, 52] <- xx[3, 2]
        df[k, 53] <- xx[4, 2]
        df[k, 54] <- sd(zz$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # ALL_1st_w_weak_move_1_mean / sd
        xx <- as.array(summary(zz$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 55] <- xx[4, 2]
        df[k, 56] <- sd(zz$w.delta, na.rm = TRUE)
        
        # ALL_1st_w_weak_move_2_mean / sd
        xx <- as.array(summary(zz$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 57] <- xx[4, 2]
        df[k, 58] <- sd(zz$w.weak.move.2, na.rm = TRUE )
        
        # ALL_1st_w_weak_move_3_mean / sd
        xx <- as.array(summary(zz$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 59] <- xx[4, 2]
        df[k, 60] <- sd(zz$w.weak.move.3, na.rm = TRUE)
        
        # ALL_1st_w_weak_move_4_mean / sd
        xx <- as.array(summary(zz$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 61] <- xx[4, 2]
        df[k, 62] <- sd(zz$w.weak.move.4, na.rm = TRUE)
        
        # ALL_1st_w_weak_move_5_mean / sd
        xx <- as.array(summary(zz$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 63] <- xx[4, 2]
        df[k, 64] <- sd(zz$w.weak.move.5, na.rm = TRUE)
        
        # ALL_1st_w_weak_move_6_mean / sd
        xx <- as.array(summary(zz$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 65] <- xx[4, 2]
        df[k, 66] <- sd(zz$w.weak.move.6, na.rm = TRUE)
        
        # ALL_1st_w_weak_move_7_mean / sd
        xx <- as.array(summary(zz$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 67] <- xx[4, 2]
        df[k, 68] <- sd(zz$w.weak.move.7, na.rm = TRUE)
        
        # ALL_1st_w_weak_move_8_mean / sd
        xx <- as.array(summary(zz$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 69] <- xx[4, 2]
        df[k, 70] <- sd(zz$w.weak.move.8, na.rm = TRUE)
        
        # ALL_1st_w_weak_move_9_mean / sd
        xx <- as.array(summary(zz$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 71] <- xx[4, 2]
        df[k, 72] <- sd(zz$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # ALL_1st_b_prev_score_mean / sd
        xx <- as.array(summary(zz$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 73] <- xx[3, 2]
        df[k, 74] <- xx[4, 2]
        df[k, 75] <- sd(zz$b.prev.score, na.rm = TRUE)
        
        # ALL_1st_b_delta_median / mean / sd
        xx <- as.array(summary(zz$b.delta))
        xx <- as.data.frame(xx)
        df[k, 76] <- xx[3, 2]
        df[k, 77] <- xx[4, 2]
        df[k, 78] <- sd(zz$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # ALL_1st_b_weak_move_1_mean / sd
        xx <- as.array(summary(zz$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 79] <- xx[4, 2]
        df[k, 80] <- sd(zz$b.weak.move.1, na.rm = TRUE)
        
        # ALL_1st_b_weak_move_2_mean / sd
        xx <- as.array(summary(zz$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 81] <- xx[4, 2]
        df[k, 82] <- sd(zz$b.weak.move.2, na.rm = TRUE)
        
        # ALL_1st_b_weak_move_3_mean / sd
        xx <- as.array(summary(zz$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 83] <- xx[4, 2]
        df[k, 84] <- sd(zz$b.weak.move.3, na.rm = TRUE)
        
        # ALL_1st_b_weak_move_4_mean / sd
        xx <- as.array(summary(zz$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 85] <- xx[4, 2]
        df[k, 86] <- sd(zz$b.weak.move.4, na.rm = TRUE)
        
        # ALL_1st_b_weak_move_5_mean / sd
        xx <- as.array(summary(zz$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 87] <- xx[4, 2]
        df[k, 88] <- sd(zz$b.weak.move.5, na.rm = TRUE)
        
        # ALL_1st_b_weak_move_6_mean / sd
        xx <- as.array(summary(zz$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 89] <- xx[4, 2]
        df[k, 90] <- sd(zz$b.weak.move.6, na.rm = TRUE)
        
        # ALL_1st_b_weak_move_7_mean / sd
        xx <- as.array(summary(zz$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 91] <- xx[4, 2]
        df[k, 92] <- sd(zz$b.weak.move.7, na.rm = TRUE)
        
        # ALL_1st_b_weak_move_8_mean / sd
        xx <- as.array(summary(zz$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 93] <- xx[4, 2]
        df[k, 94] <- sd(zz$b.weak.move.8, na.rm = TRUE)
        
        # ALL_1st_b_weak_move_9_mean / sd
        xx <- as.array(summary(zz$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 95] <- xx[4, 2]
        df[k, 96] <- sd(zz$b.weak.move.9, na.rm = TRUE)
        
        ######################################
        ## subset z - 2nd Qtr of game moves ##       
        ######################################
        z50 <- as.integer(nrow(z) * 0.50)
        zz <- zz[z25:z50, ]
        
        ###################
        # ALL_2nd_w_prev_score_median / mean / sd
        xx <- as.array(summary(zz$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 97] <- xx[3, 2]
        df[k, 98] <- xx[4, 2]
        df[k, 99] <- sd(zz$w.prev.score, na.rm = TRUE)
        
        # ALL_2nd_w_delta_median / mean / sd
        xx <- as.array(summary(zz$w.delta))
        xx <- as.data.frame(xx)
        df[k, 100] <- xx[3, 2]
        df[k, 101] <- xx[4, 2]
        df[k, 102] <- sd(zz$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # ALL_2nd_w_weak_move_1_mean / sd
        xx <- as.array(summary(zz$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 103] <- xx[4, 2]
        df[k, 104] <- sd(zz$w.delta, na.rm = TRUE)
        
        # ALL_2nd_w_weak_move_2_mean / sd
        xx <- as.array(summary(zz$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 105] <- xx[4, 2]
        df[k, 106] <- sd(zz$w.weak.move.2, na.rm = TRUE )
        
        # ALL_2nd_w_weak_move_3_mean / sd
        xx <- as.array(summary(zz$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 107] <- xx[4, 2]
        df[k, 108] <- sd(zz$w.weak.move.3, na.rm = TRUE)
        
        # ALL_2nd_w_weak_move_4_mean / sd
        xx <- as.array(summary(zz$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 109] <- xx[4, 2]
        df[k, 110] <- sd(zz$w.weak.move.4, na.rm = TRUE)
        
        # ALL_2nd_w_weak_move_5_mean / sd
        xx <- as.array(summary(zz$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 111] <- xx[4, 2]
        df[k, 112] <- sd(zz$w.weak.move.5, na.rm = TRUE)
        
        # ALL_2nd_w_weak_move_6_mean / sd
        xx <- as.array(summary(zz$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 113] <- xx[4, 2]
        df[k, 114] <- sd(zz$w.weak.move.6, na.rm = TRUE)
        
        # ALL_2nd_w_weak_move_7_mean / sd
        xx <- as.array(summary(zz$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 115] <- xx[4, 2]
        df[k, 116] <- sd(zz$w.weak.move.7, na.rm = TRUE)
        
        # ALL_2nd_w_weak_move_8_mean / sd
        xx <- as.array(summary(zz$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 117] <- xx[4, 2]
        df[k, 118] <- sd(zz$w.weak.move.8, na.rm = TRUE)
        
        # ALL_2nd_w_weak_move_9_mean / sd
        xx <- as.array(summary(zz$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 119] <- xx[4, 2]
        df[k, 120] <- sd(zz$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # ALL_2nd_b_prev_score_median / mean / sd
        xx <- as.array(summary(zz$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 121] <- xx[3, 2]
        df[k, 122] <- xx[4, 2]
        df[k, 123] <- sd(zz$b.prev.score, na.rm = TRUE)
        
        # ALL_2nd_b_delta_median / mean / sd
        xx <- as.array(summary(zz$b.delta))
        xx <- as.data.frame(xx)
        df[k, 124] <- xx[3, 2]
        df[k, 125] <- xx[4, 2]
        df[k, 126] <- sd(zz$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # ALL_2nd_b_weak_move_1_mean / sd
        xx <- as.array(summary(zz$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 127] <- xx[4, 2]
        df[k, 128] <- sd(zz$b.weak.move.1, na.rm = TRUE)
        
        # ALL_2nd_b_weak_move_2_mean / sd
        xx <- as.array(summary(zz$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 129] <- xx[4, 2]
        df[k, 130] <- sd(zz$b.weak.move.2, na.rm = TRUE)
        
        # ALL_2nd_b_weak_move_3_mean / sd
        xx <- as.array(summary(zz$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 131] <- xx[4, 2]
        df[k, 132] <- sd(zz$b.weak.move.3, na.rm = TRUE)
        
        # ALL_2nd_b_weak_move_4_mean / sd
        xx <- as.array(summary(zz$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 133] <- xx[4, 2]
        df[k, 134] <- sd(zz$b.weak.move.4, na.rm = TRUE)
        
        # ALL_2nd_b_weak_move_5_mean / sd
        xx <- as.array(summary(zz$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 135] <- xx[4, 2]
        df[k, 136] <- sd(zz$b.weak.move.5, na.rm = TRUE)
        
        # ALL_2nd_b_weak_move_6_mean / sd
        xx <- as.array(summary(zz$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 137] <- xx[4, 2]
        df[k, 138] <- sd(zz$b.weak.move.6, na.rm = TRUE)
        
        # ALL_2nd_b_weak_move_7_mean / sd
        xx <- as.array(summary(zz$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 139] <- xx[4, 2]
        df[k, 140] <- sd(zz$b.weak.move.7, na.rm = TRUE)
        
        # ALL_2nd_b_weak_move_8_mean / sd
        xx <- as.array(summary(zz$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 141] <- xx[4, 2]
        df[k, 142] <- sd(zz$b.weak.move.8, na.rm = TRUE)
        
        # ALL_2nd_b_weak_move_9_mean / sd
        xx <- as.array(summary(zz$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 143] <- xx[4, 2]
        df[k, 144] <- sd(zz$b.weak.move.9, na.rm = TRUE)
        
        
        #######################################
        ## subset z - 3rd Qtr of game moves  ##         
        #######################################
        z75 <- as.integer(nrow(z) * 0.75)
        zz <- z[z50:z75, ]
        
        ###################
        # ALL_3rd_w_prev_score_median / mean / sd
        xx <- as.array(summary(zz$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 145] <- xx[3, 2]
        df[k, 146] <- xx[4, 2]
        df[k, 147] <- sd(zz$w.prev.score, na.rm = TRUE)
        
        # ALL_3rd_w_delta_median / mean / sd
        xx <- as.array(summary(zz$w.delta))
        xx <- as.data.frame(xx)
        df[k, 148] <- xx[3, 2]
        df[k, 149] <- xx[4, 2]
        df[k, 150] <- sd(zz$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # ALL_3rd_w_weak_move_1_mean / sd
        xx <- as.array(summary(zz$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 151] <- xx[4, 2]
        df[k, 152] <- sd(zz$w.delta, na.rm = TRUE)
        
        # ALL_3rd_w_weak_move_2_mean / sd
        xx <- as.array(summary(zz$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 153] <- xx[4, 2]
        df[k, 154] <- sd(zz$w.weak.move.2, na.rm = TRUE )
        
        # ALL_3rd_w_weak_move_3_mean / sd
        xx <- as.array(summary(zz$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 155] <- xx[4, 2]
        df[k, 156] <- sd(zz$w.weak.move.3, na.rm = TRUE)
        
        # ALL_3rd_w_weak_move_4_mean / sd
        xx <- as.array(summary(zz$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 157] <- xx[4, 2]
        df[k, 158] <- sd(zz$w.weak.move.4, na.rm = TRUE)
        
        # ALL_3rd_w_weak_move_5_mean / sd
        xx <- as.array(summary(zz$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 159] <- xx[4, 2]
        df[k, 160] <- sd(zz$w.weak.move.5, na.rm = TRUE)
        
        # ALL_3rd_w_weak_move_6_mean / sd
        xx <- as.array(summary(zz$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 161] <- xx[4, 2]
        df[k, 162] <- sd(zz$w.weak.move.6, na.rm = TRUE)
        
        # ALL_3rd_w_weak_move_7_mean / sd
        xx <- as.array(summary(zz$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 163] <- xx[4, 2]
        df[k, 164] <- sd(zz$w.weak.move.7, na.rm = TRUE)
        
        # ALL_3rd_w_weak_move_8_mean / sd
        xx <- as.array(summary(zz$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 165] <- xx[4, 2]
        df[k, 166] <- sd(zz$w.weak.move.8, na.rm = TRUE)
        
        # ALL_3rd_w_weak_move_9_mean / sd
        xx <- as.array(summary(zz$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 167] <- xx[4, 2]
        df[k, 168] <- sd(zz$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # ALL_3rd_b_prev_score_median / mean / sd
        xx <- as.array(summary(zz$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 169] <- xx[3, 2]
        df[k, 170] <- xx[4, 2]
        df[k, 171] <- sd(zz$b.prev.score, na.rm = TRUE)
        
        # ALL_3rd_b_delta_median / mean / sd
        xx <- as.array(summary(zz$b.delta))
        xx <- as.data.frame(xx)
        df[k, 172] <- xx[3, 2]
        df[k, 173] <- xx[4, 2]
        df[k, 174] <- sd(zz$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # ALL_3rd_b_weak_move_1_mean / sd
        xx <- as.array(summary(zz$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 175] <- xx[4, 2]
        df[k, 176] <- sd(zz$b.weak.move.1, na.rm = TRUE)
        
        # ALL_3rd_b_weak_move_2_mean / sd
        xx <- as.array(summary(zz$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 177] <- xx[4, 2]
        df[k, 178] <- sd(zz$b.weak.move.2, na.rm = TRUE)
        
        # ALL_3rd_b_weak_move_3_mean / sd
        xx <- as.array(summary(zz$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 179] <- xx[4, 2]
        df[k, 180] <- sd(zz$b.weak.move.3, na.rm = TRUE)
        
        # ALL_3rd_b_weak_move_4_mean / sd
        xx <- as.array(summary(zz$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 181] <- xx[4, 2]
        df[k, 182] <- sd(zz$b.weak.move.4, na.rm = TRUE)
        
        # ALL_3rd_b_weak_move_5_mean / sd
        xx <- as.array(summary(zz$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 183] <- xx[4, 2]
        df[k, 184] <- sd(zz$b.weak.move.5, na.rm = TRUE)
        
        # ALL_3rd_b_weak_move_6_mean / sd
        xx <- as.array(summary(zz$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 185] <- xx[4, 2]
        df[k, 186] <- sd(zz$b.weak.move.6, na.rm = TRUE)
        
        # ALL_3rd_b_weak_move_7_mean / sd
        xx <- as.array(summary(zz$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 187] <- xx[4, 2]
        df[k, 188] <- sd(zz$b.weak.move.7, na.rm = TRUE)
        
        # ALL_3rd_b_weak_move_8_mean / sd
        xx <- as.array(summary(zz$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 189] <- xx[4, 2]
        df[k, 190] <- sd(zz$b.weak.move.8, na.rm = TRUE)
        
        # ALL_3rd_b_weak_move_9_mean / sd
        xx <- as.array(summary(zz$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 191] <- xx[4, 2]
        df[k, 192] <- sd(zz$b.weak.move.9, na.rm = TRUE)
        
        
        #######################
        # Cumulative 0-50
        ########################
        ## subset z - 3rd Qtr of game moves         
        #z75 <- as.integer(nrow(z) * 0.75)
        zz <- z[1:z50, ]
        
        ###################
        # ALL_0-50_w_prev_score_median / mean / sd
        xx <- as.array(summary(zz$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 193] <- xx[3, 2]
        df[k, 194] <- xx[4, 2]
        df[k, 195] <- sd(zz$w.prev.score, na.rm = TRUE)
        
        # ALL_0-50_w_delta_median / mean / sd
        xx <- as.array(summary(zz$w.delta))
        xx <- as.data.frame(xx)
        df[k, 196] <- xx[3, 2]
        df[k, 197] <- xx[4, 2]
        df[k, 198] <- sd(zz$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # ALL_0-50_w_weak_move_1_mean / sd
        xx <- as.array(summary(zz$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 199] <- xx[4, 2]
        df[k, 200] <- sd(zz$w.delta, na.rm = TRUE)
        
        # ALL_0-50_w_weak_move_2_mean / sd
        xx <- as.array(summary(zz$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 201] <- xx[4, 2]
        df[k, 202] <- sd(zz$w.weak.move.2, na.rm = TRUE )
        
        # ALL_0-50_w_weak_move_3_mean / sd
        xx <- as.array(summary(zz$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 203] <- xx[4, 2]
        df[k, 204] <- sd(zz$w.weak.move.3, na.rm = TRUE)
        
        # ALL_0-50_w_weak_move_4_mean / sd
        xx <- as.array(summary(zz$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 205] <- xx[4, 2]
        df[k, 206] <- sd(zz$w.weak.move.4, na.rm = TRUE)
        
        # ALL_0-50_w_weak_move_5_mean / sd
        xx <- as.array(summary(zz$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 207] <- xx[4, 2]
        df[k, 208] <- sd(zz$w.weak.move.5, na.rm = TRUE)
        
        # ALL_0-50_w_weak_move_6_mean / sd
        xx <- as.array(summary(zz$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 209] <- xx[4, 2]
        df[k, 210] <- sd(zz$w.weak.move.6, na.rm = TRUE)
        
        # ALL_0-50_w_weak_move_7_mean / sd
        xx <- as.array(summary(zz$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 211] <- xx[4, 2]
        df[k, 212] <- sd(zz$w.weak.move.7, na.rm = TRUE)
        
        # ALL_0-50_w_weak_move_8_mean / sd
        xx <- as.array(summary(zz$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 213] <- xx[4, 2]
        df[k, 214] <- sd(zz$w.weak.move.8, na.rm = TRUE)
        
        # ALL_0-50_w_weak_move_9_mean / sd
        xx <- as.array(summary(zz$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 215] <- xx[4, 2]
        df[k, 216] <- sd(zz$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # ALL_0-50_b_prev_score_mean / sd
        xx <- as.array(summary(zz$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 217] <- xx[3, 2]
        df[k, 218] <- xx[4, 2]
        df[k, 219] <- sd(zz$b.prev.score, na.rm = TRUE)
        
        # ALL_0-50_b_delta_mean / sd
        xx <- as.array(summary(zz$b.delta))
        xx <- as.data.frame(xx)
        df[k, 220] <- xx[3, 2]
        df[k, 221] <- xx[4, 2]
        df[k, 222] <- sd(zz$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # ALL_0-50_b_weak_move_1_mean / sd
        xx <- as.array(summary(zz$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 223] <- xx[4, 2]
        df[k, 224] <- sd(zz$b.weak.move.1, na.rm = TRUE)
        
        # ALL_0-50_b_weak_move_2_mean / sd
        xx <- as.array(summary(zz$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 225] <- xx[4, 2]
        df[k, 226] <- sd(zz$b.weak.move.2, na.rm = TRUE)
        
        # ALL_0-50_b_weak_move_3_mean / sd
        xx <- as.array(summary(zz$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 227] <- xx[4, 2]
        df[k, 228] <- sd(zz$b.weak.move.3, na.rm = TRUE)
        
        # ALL_0-50_b_weak_move_4_mean / sd
        xx <- as.array(summary(zz$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 229] <- xx[4, 2]
        df[k, 230] <- sd(zz$b.weak.move.4, na.rm = TRUE)
        
        # ALL_0-50_b_weak_move_5_mean / sd
        xx <- as.array(summary(zz$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 231] <- xx[4, 2]
        df[k, 232] <- sd(zz$b.weak.move.5, na.rm = TRUE)
        
        # ALL_0-50_b_weak_move_6_mean / sd
        xx <- as.array(summary(zz$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 233] <- xx[4, 2]
        df[k, 234] <- sd(zz$b.weak.move.6, na.rm = TRUE)
        
        # ALL_0-50_b_weak_move_7_mean / sd
        xx <- as.array(summary(zz$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 235] <- xx[4, 2]
        df[k, 236] <- sd(zz$b.weak.move.7, na.rm = TRUE)
        
        # ALL_0-50_b_weak_move_8_mean / sd
        xx <- as.array(summary(zz$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 237] <- xx[4, 2]
        df[k, 238] <- sd(zz$b.weak.move.8, na.rm = TRUE)
        
        # ALL_0-50_b_weak_move_9_mean / sd
        xx <- as.array(summary(zz$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 239] <- xx[4, 2]
        df[k, 240] <- sd(zz$b.weak.move.9, na.rm = TRUE)
        
        #########################
        #######################
        # Cumulative 0-75
        ########################
        ## subset z - 3rd Qtr of game moves         
        #z75 <- as.integer(nrow(z) * 0.75)
        zz <- z[1:z75, ]
        
        ###################
        # ALL_0-75_w_prev_score_median / mean / sd
        xx <- as.array(summary(zz$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 241] <- xx[3, 2]
        df[k, 242] <- xx[4, 2]
        df[k, 243] <- sd(zz$w.prev.score, na.rm = TRUE)
        
        # ALL_0-75_w_delta_median / mean / sd
        xx <- as.array(summary(zz$w.delta))
        xx <- as.data.frame(xx)
        df[k, 244] <- xx[3, 2]
        df[k, 245] <- xx[4, 2]
        df[k, 246] <- sd(zz$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # ALL_0-75_w_weak_move_1_mean / sd
        xx <- as.array(summary(zz$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 247] <- xx[4, 2]
        df[k, 248] <- sd(zz$w.delta, na.rm = TRUE)
        
        # ALL_0-75_w_weak_move_2_mean / sd
        xx <- as.array(summary(zz$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 249] <- xx[4, 2]
        df[k, 250] <- sd(zz$w.weak.move.2, na.rm = TRUE )
        
        # ALL_0-75_w_weak_move_3_mean / sd
        xx <- as.array(summary(zz$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 251] <- xx[4, 2]
        df[k, 252] <- sd(zz$w.weak.move.3, na.rm = TRUE)
        
        # ALL_0-75_w_weak_move_4_mean / sd
        xx <- as.array(summary(zz$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 253] <- xx[4, 2]
        df[k, 254] <- sd(zz$w.weak.move.4, na.rm = TRUE)
        
        # ALL_0-75_w_weak_move_5_mean / sd
        xx <- as.array(summary(zz$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 255] <- xx[4, 2]
        df[k, 256] <- sd(zz$w.weak.move.5, na.rm = TRUE)
        
        # ALL_0-75_w_weak_move_6_mean / sd
        xx <- as.array(summary(zz$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 257] <- xx[4, 2]
        df[k, 258] <- sd(zz$w.weak.move.6, na.rm = TRUE)
        
        # ALL_0-75_w_weak_move_7_mean / sd
        xx <- as.array(summary(zz$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 259] <- xx[4, 2]
        df[k, 260] <- sd(zz$w.weak.move.7, na.rm = TRUE)
        
        # ALL_0-75_w_weak_move_8_mean / sd
        xx <- as.array(summary(zz$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 261] <- xx[4, 2]
        df[k, 262] <- sd(zz$w.weak.move.8, na.rm = TRUE)
        
        # ALL_0-75_w_weak_move_9_mean / sd
        xx <- as.array(summary(zz$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 263] <- xx[4, 2]
        df[k, 264] <- sd(zz$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # ALL_0-75_b_prev_score_mean / sd
        xx <- as.array(summary(zz$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 265] <- xx[3, 2]
        df[k, 266] <- xx[4, 2]
        df[k, 267] <- sd(zz$b.prev.score, na.rm = TRUE)
        
        # ALL_0-75_b_delta_mean / sd
        xx <- as.array(summary(zz$b.delta))
        xx <- as.data.frame(xx)
        df[k, 268] <- xx[3, 2]
        df[k, 269] <- xx[4, 2]
        df[k, 270] <- sd(zz$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # ALL_0-75_b_weak_move_1_mean / sd
        xx <- as.array(summary(zz$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 271] <- xx[4, 2]
        df[k, 272] <- sd(zz$b.weak.move.1, na.rm = TRUE)
        
        # ALL_0-75_b_weak_move_2_mean / sd
        xx <- as.array(summary(zz$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 273] <- xx[4, 2]
        df[k, 274] <- sd(zz$b.weak.move.2, na.rm = TRUE)
        
        # ALL_0-75_b_weak_move_3_mean / sd
        xx <- as.array(summary(zz$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 275] <- xx[4, 2]
        df[k, 276] <- sd(zz$b.weak.move.3, na.rm = TRUE)
        
        # ALL_0-75_b_weak_move_4_mean / sd
        xx <- as.array(summary(zz$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 277] <- xx[4, 2]
        df[k, 278] <- sd(zz$b.weak.move.4, na.rm = TRUE)
        
        # ALL_0-75_b_weak_move_5_mean / sd
        xx <- as.array(summary(zz$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 279] <- xx[4, 2]
        df[k, 280] <- sd(zz$b.weak.move.5, na.rm = TRUE)
        
        # ALL_0-75_b_weak_move_6_mean / sd
        xx <- as.array(summary(zz$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 281] <- xx[4, 2]
        df[k, 282] <- sd(zz$b.weak.move.6, na.rm = TRUE)
        
        # ALL_0-75_b_weak_move_7_mean / sd
        xx <- as.array(summary(zz$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 283] <- xx[4, 2]
        df[k, 284] <- sd(zz$b.weak.move.7, na.rm = TRUE)
        
        # ALL_0-75_b_weak_move_8_mean / sd
        xx <- as.array(summary(zz$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 285] <- xx[4, 2]
        df[k, 286] <- sd(zz$b.weak.move.8, na.rm = TRUE)
        
        # ALL_0-75_b_weak_move_9_mean / sd
        xx <- as.array(summary(zz$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 287] <- xx[4, 2]
        df[k, 288] <- sd(zz$b.weak.move.9, na.rm = TRUE)
        
        
        #######################
        #######################
        # Get undecided:
        #######################
        # Get absolute score of game score -----------
        zz <- z
        zz$even.game <- ifelse(zz[, 5] < even_game, 1, 0)
        zz <- subset(zz, zz$even.game == 1)
        
        ###########################################
        ## Get delta Summary: 1st Qu., Median, 3rd Qu.
        # undec_ALL_w_prev_score_median /mean / sd
        xx <- as.array(summary(zz$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 289] <- xx[3, 2]
        df[k, 290] <- xx[4, 2]
        df[k, 291] <- sd(zz$w.prev.score, na.rm = TRUE)
        
        # undec_ALL_w_delta_median / mean / sd
        xx <- as.array(summary(zz$w.delta))
        xx <- as.data.frame(xx)
        df[k, 292] <- xx[3, 2]
        df[k, 293] <- xx[4, 2]
        df[k, 294] <- sd(zz$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # undec_ALL_w_weak_move_1_mean / sd
        xx <- as.array(summary(zz$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 295] <- xx[4, 2]
        df[k, 296] <- sd(zz$w.delta, na.rm = TRUE)
        
        # undec_ALL_w_weak_move_2_mean / sd
        xx <- as.array(summary(zz$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 297] <- xx[4, 2]
        df[k, 298] <- sd(zz$w.weak.move.2, na.rm = TRUE )
        
        # undec_ALL_w_weak_move_3_mean / sd
        xx <- as.array(summary(zz$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 299] <- xx[4, 2]
        df[k, 300] <- sd(zz$w.weak.move.3, na.rm = TRUE)
        
        # undec_ALL_w_weak_move_4_mean / sd
        xx <- as.array(summary(zz$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 301] <- xx[4, 2]
        df[k, 302] <- sd(zz$w.weak.move.4, na.rm = TRUE)
        
        # undec_ALL_w_weak_move_5_mean / sd
        xx <- as.array(summary(zz$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 303] <- xx[4, 2]
        df[k, 304] <- sd(zz$w.weak.move.5, na.rm = TRUE)
        
        # undec_ALL_w_weak_move_6_mean / sd
        xx <- as.array(summary(zz$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 305] <- xx[4, 2]
        df[k, 306] <- sd(zz$w.weak.move.6, na.rm = TRUE)
        
        # undec_ALL_w_weak_move_7_mean / sd
        xx <- as.array(summary(zz$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 307] <- xx[4, 2]
        df[k, 308] <- sd(zz$w.weak.move.7, na.rm = TRUE)
        
        # undec_ALL_w_weak_move_8_mean / sd
        xx <- as.array(summary(zz$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 309] <- xx[4, 2]
        df[k, 310] <- sd(zz$w.weak.move.8, na.rm = TRUE)
        
        # undec_ALL_w_weak_move_9_mean / sd
        xx <- as.array(summary(zz$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 311] <- xx[4, 2]
        df[k, 312] <- sd(zz$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # undec_ALL_b_prev_score_median / mean / sd
        xx <- as.array(summary(zz$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 313] <- xx[3, 2]
        df[k, 314] <- xx[4, 2]
        df[k, 315] <- sd(zz$b.prev.score, na.rm = TRUE)
        
        # undec_ALL_b_delta_median / mean / sd
        xx <- as.array(summary(zz$b.delta))
        xx <- as.data.frame(xx)
        df[k, 316] <- xx[3, 2]
        df[k, 317] <- xx[4, 2]
        df[k, 318] <- sd(zz$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # undec_ALL_b_weak_move_1_mean / sd
        xx <- as.array(summary(zz$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 319] <- xx[4, 2]
        df[k, 320] <- sd(zz$b.weak.move.1, na.rm = TRUE)
        
        # undec_ALL_b_weak_move_2_mean / sd
        xx <- as.array(summary(zz$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 321] <- xx[4, 2]
        df[k, 322] <- sd(zz$b.weak.move.2, na.rm = TRUE)
        
        # undec_ALL_b_weak_move_3_mean / sd
        xx <- as.array(summary(zz$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 323] <- xx[4, 2]
        df[k, 324] <- sd(zz$b.weak.move.3, na.rm = TRUE)
        
        # undec_ALL_b_weak_move_4_mean / sd
        xx <- as.array(summary(zz$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 325] <- xx[4, 2]
        df[k, 326] <- sd(zz$b.weak.move.4, na.rm = TRUE)
        
        # undec_ALL_b_weak_move_5_mean / sd
        xx <- as.array(summary(zz$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 327] <- xx[4, 2]
        df[k, 328] <- sd(zz$b.weak.move.5, na.rm = TRUE)
        
        # undec_ALL_b_weak_move_6_mean / sd
        xx <- as.array(summary(zz$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 329] <- xx[4, 2]
        df[k, 330] <- sd(zz$b.weak.move.6, na.rm = TRUE)
        
        # undec_ALL_b_weak_move_7_mean / sd
        xx <- as.array(summary(zz$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 331] <- xx[4, 2]
        df[k, 332] <- sd(zz$b.weak.move.7, na.rm = TRUE)
        
        # undec_ALL_b_weak_move_8_mean / sd
        xx <- as.array(summary(zz$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 333] <- xx[4, 2]
        df[k, 334] <- sd(zz$b.weak.move.8, na.rm = TRUE)
        
        # undec_ALL_b_weak_move_9_mean / sd
        xx <- as.array(summary(zz$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 335] <- xx[4, 2]
        df[k, 336] <- sd(zz$b.weak.move.9, na.rm = TRUE)
        
        
        #data[complete.cases(data$Shoulders), ]
        # df[complete.cases(df[k, ])]
        
        ####################################
        ## divide above into 1st quarter  ##
        ####################################
        ## subset z - 1st Qtr of game moves         
        z25 <- as.integer(nrow(zz) * 0.25)
        zzz <- zz[1:z25, ]
        
        ###################
        # undec_1st_w_prev_score_median / mean / sd
        xx <- as.array(summary(zzz$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 337] <- xx[3, 2]
        df[k, 338] <- xx[4, 2]
        df[k, 339] <- sd(zzz$w.prev.score, na.rm = TRUE)
        
        # undec_1st_w_delta_median / mean / sd
        xx <- as.array(summary(zzz$w.delta))
        xx <- as.data.frame(xx)
        df[k, 340] <- xx[3, 2]
        df[k, 341] <- xx[4, 2]
        df[k, 342] <- sd(zzz$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # undec_1st_w_weak_move_1_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 343] <- xx[4, 2]
        df[k, 344] <- sd(zzz$w.delta, na.rm = TRUE)
        
        # undec_1st_w_weak_move_2_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 345] <- xx[4, 2]
        df[k, 346] <- sd(zzz$w.weak.move.2, na.rm = TRUE )
        
        # undec_1st_w_weak_move_3_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 347] <- xx[4, 2]
        df[k, 348] <- sd(zzz$w.weak.move.3, na.rm = TRUE)
        
        # undec_1st_w_weak_move_4_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 349] <- xx[4, 2]
        df[k, 350] <- sd(zzz$w.weak.move.4, na.rm = TRUE)
        
        # undec_1st_w_weak_move_5_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 351] <- xx[4, 2]
        df[k, 352] <- sd(zzz$w.weak.move.5, na.rm = TRUE)
        
        # undec_1st_w_weak_move_6_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 353] <- xx[4, 2]
        df[k, 354] <- sd(zzz$w.weak.move.6, na.rm = TRUE)
        
        # undec_1st_w_weak_move_7_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 355] <- xx[4, 2]
        df[k, 356] <- sd(zzz$w.weak.move.7, na.rm = TRUE)
        
        # undec_1st_w_weak_move_8_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 357] <- xx[4, 2]
        df[k, 358] <- sd(zzz$w.weak.move.8, na.rm = TRUE)
        
        # undec_1st_w_weak_move_9_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 359] <- xx[4, 2]
        df[k, 360] <- sd(zzz$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # undec_1st_b_prev_score_median / mean / sd
        xx <- as.array(summary(zzz$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 361] <- xx[3, 2]
        df[k, 362] <- xx[4, 2]
        df[k, 363] <- sd(zzz$b.prev.score, na.rm = TRUE)
        
        # undec_1st_b_delta_median / mean / sd
        xx <- as.array(summary(zzz$b.delta))
        xx <- as.data.frame(xx)
        df[k, 364] <- xx[3, 2]
        df[k, 365] <- xx[4, 2]
        df[k, 366] <- sd(zzz$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # undec_1st_b_weak_move_1_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 367] <- xx[4, 2]
        df[k, 368] <- sd(zzz$b.weak.move.1, na.rm = TRUE)
        
        # undec_1st_b_weak_move_2_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 369] <- xx[4, 2]
        df[k, 370] <- sd(zzz$b.weak.move.2, na.rm = TRUE)
        
        # undec_1st_b_weak_move_3_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 371] <- xx[4, 2]
        df[k, 372] <- sd(zzz$b.weak.move.3, na.rm = TRUE)
        
        # undec_1st_b_weak_move_4_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 373] <- xx[4, 2]
        df[k, 374] <- sd(zzz$b.weak.move.4, na.rm = TRUE)
        
        # undec_1st_b_weak_move_5_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 375] <- xx[4, 2]
        df[k, 376] <- sd(zzz$b.weak.move.5, na.rm = TRUE)
        
        # undec_1st_b_weak_move_6_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 377] <- xx[4, 2]
        df[k, 378] <- sd(zzz$b.weak.move.6, na.rm = TRUE)
        
        # undec_1st_b_weak_move_7_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 379] <- xx[4, 2]
        df[k, 380] <- sd(zzz$b.weak.move.7, na.rm = TRUE)
        
        # undec_1st_b_weak_move_8_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 381] <- xx[4, 2]
        df[k, 382] <- sd(zzz$b.weak.move.8, na.rm = TRUE)
        
        # undec_1st_b_weak_move_9_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 383] <- xx[4, 2]
        df[k, 384] <- sd(zzz$b.weak.move.9, na.rm = TRUE)
        
        ########################
        ########################
        ## subset z - 2nd Qtr of game moves         
        z50 <- as.integer(nrow(zz) * 0.50)
        zzz <- zz[z25:z50, ]
        
        ###################
        # undec_2nd_w_prev_score_median / mean / sd
        xx <- as.array(summary(zzz$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 385] <- xx[3, 2]
        df[k, 386] <- xx[4, 2]
        df[k, 387] <- sd(zzz$w.prev.score, na.rm = TRUE)
        
        # undec_2nd_w_delta_median / mean / sd
        xx <- as.array(summary(zzz$w.delta))
        xx <- as.data.frame(xx)
        df[k, 388] <- xx[3, 2]
        df[k, 389] <- xx[4, 2]
        df[k, 390] <- sd(zzz$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # undec_2nd_w_weak_move_1_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 391] <- xx[4, 2]
        df[k, 392] <- sd(zzz$w.delta, na.rm = TRUE)
        
        # undec_2nd_w_weak_move_2_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 393] <- xx[4, 2]
        df[k, 394] <- sd(zzz$w.weak.move.2, na.rm = TRUE )
        
        # undec_2nd_w_weak_move_3_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 395] <- xx[4, 2]
        df[k, 396] <- sd(zzz$w.weak.move.3, na.rm = TRUE)
        
        # undec_2nd_w_weak_move_4_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 397] <- xx[4, 2]
        df[k, 398] <- sd(zzz$w.weak.move.4, na.rm = TRUE)
        
        # undec_2nd_w_weak_move_5_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 399] <- xx[4, 2]
        df[k, 400] <- sd(zzz$w.weak.move.5, na.rm = TRUE)
        
        # undec_2nd_w_weak_move_6_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 401] <- xx[4, 2]
        df[k, 402] <- sd(zzz$w.weak.move.6, na.rm = TRUE)
        
        # undec_2nd_w_weak_move_7_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 403] <- xx[4, 2]
        df[k, 404] <- sd(zzz$w.weak.move.7, na.rm = TRUE)
        
        # undec_2nd_w_weak_move_8_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 405] <- xx[4, 2]
        df[k, 406] <- sd(zzz$w.weak.move.8, na.rm = TRUE)
        
        # undec_2nd_w_weak_move_9_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 407] <- xx[4, 2]
        df[k, 408] <- sd(zzz$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # undec_2nd_b_prev_score_median / mean / sd
        xx <- as.array(summary(zzz$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 409] <- xx[3, 2]
        df[k, 410] <- xx[4, 2]
        df[k, 411] <- sd(zzz$b.prev.score, na.rm = TRUE)
        
        # undec_2nd_b_delta_median / mean / sd
        xx <- as.array(summary(zzz$b.delta))
        xx <- as.data.frame(xx)
        df[k, 412] <- xx[3, 2]
        df[k, 413] <- xx[4, 2]
        df[k, 414] <- sd(zzz$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # undec_2nd_b_weak_move_1_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 415] <- xx[4, 2]
        df[k, 416] <- sd(zzz$b.weak.move.1, na.rm = TRUE)
        
        # undec_2nd_b_weak_move_2_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 417] <- xx[4, 2]
        df[k, 418] <- sd(zzz$b.weak.move.2, na.rm = TRUE)
        
        # undec_2nd_b_weak_move_3_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 419] <- xx[4, 2]
        df[k, 420] <- sd(zzz$b.weak.move.3, na.rm = TRUE)
        
        # undec_2nd_b_weak_move_4_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 421] <- xx[4, 2]
        df[k, 422] <- sd(zzz$b.weak.move.4, na.rm = TRUE)
        
        # undec_2nd_b_weak_move_5_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 423] <- xx[4, 2]
        df[k, 424] <- sd(zzz$b.weak.move.5, na.rm = TRUE)
        
        # undec_2nd_b_weak_move_6_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 425] <- xx[4, 2]
        df[k, 426] <- sd(zzz$b.weak.move.6, na.rm = TRUE)
        
        # undec_2nd_b_weak_move_7_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 427] <- xx[4, 2]
        df[k, 428] <- sd(zzz$b.weak.move.7, na.rm = TRUE)
        
        # undec_2nd_b_weak_move_8_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 429] <- xx[4, 2]
        df[k, 430] <- sd(zzz$b.weak.move.8, na.rm = TRUE)
        
        # undec_2nd_b_weak_move_9_mean / sd 
        xx <- as.array(summary(zzz$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 431] <- xx[4, 2]
        df[k, 432] <- sd(zzz$b.weak.move.9, na.rm = TRUE)
        
        #######################################
        ## subset z - 3rd Qtr of game moves  ##       
        #######################################
        z75 <- as.integer(nrow(zz) * 0.75)
        zzz <- zz[z50:z75, ]
        
        ###################
        # undec_3rd_w_prev_score_median / mean / sd
        xx <- as.array(summary(zzz$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 433] <- xx[3, 2]
        df[k, 434] <- xx[4, 2]
        df[k, 435] <- sd(zzz$w.prev.score, na.rm = TRUE)
        
        # undec_3rd_w_delta_median / mean / sd
        xx <- as.array(summary(zzz$w.delta))
        xx <- as.data.frame(xx)
        df[k, 436] <- xx[3, 2]
        df[k, 437] <- xx[4, 2]
        df[k, 438] <- sd(zzz$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # undec_3rd_w_weak_move_1_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 439] <- xx[4, 2]
        df[k, 440] <- sd(zzz$w.delta, na.rm = TRUE)
        
        # undec_3rd_w_weak_move_2_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 441] <- xx[4, 2]
        df[k, 442] <- sd(zzz$w.weak.move.2, na.rm = TRUE )
        
        # undec_3rd_w_weak_move_3_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 443] <- xx[4, 2]
        df[k, 444] <- sd(zzz$w.weak.move.3, na.rm = TRUE)
        
        # undec_3rd_w_weak_move_4_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 445] <- xx[4, 2]
        df[k, 446] <- sd(zzz$w.weak.move.4, na.rm = TRUE)
        
        # undec_3rd_w_weak_move_5_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 447] <- xx[4, 2]
        df[k, 448] <- sd(zzz$w.weak.move.5, na.rm = TRUE)
        
        # undec_3rd_w_weak_move_6_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 449] <- xx[4, 2]
        df[k, 450] <- sd(zzz$w.weak.move.6, na.rm = TRUE)
        
        # undec_3rd_w_weak_move_7_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 451] <- xx[4, 2]
        df[k, 452] <- sd(zzz$w.weak.move.7, na.rm = TRUE)
        
        # undec_3rd_w_weak_move_8_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 453] <- xx[4, 2]
        df[k, 454] <- sd(zzz$w.weak.move.8, na.rm = TRUE)
        
        # undec_3rd_w_weak_move_9_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 455] <- xx[4, 2]
        df[k, 456] <- sd(zzz$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # undec_3rd_b_prev_score_median / mean / sd
        xx <- as.array(summary(zzz$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 457] <- xx[3, 2]
        df[k, 458] <- xx[4, 2]
        df[k, 459] <- sd(zzz$b.prev.score, na.rm = TRUE)
        
        # undec_3rd_b_delta_median / mean / sd
        xx <- as.array(summary(zzz$b.delta))
        xx <- as.data.frame(xx)
        df[k, 460] <- xx[3, 2]
        df[k, 461] <- xx[4, 2]
        df[k, 462] <- sd(zzz$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # undec_3rd_b_weak_move_1_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 463] <- xx[4, 2]
        df[k, 464] <- sd(zzz$b.weak.move.1, na.rm = TRUE)
        
        # undec_3rd_b_weak_move_2_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 465] <- xx[4, 2]
        df[k, 466] <- sd(zzz$b.weak.move.2, na.rm = TRUE)
        
        # undec_3rd_b_weak_move_3_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 467] <- xx[4, 2]
        df[k, 468] <- sd(zzz$b.weak.move.3, na.rm = TRUE)
        
        # undec_3rd_b_weak_move_4_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 469] <- xx[4, 2]
        df[k, 470] <- sd(zzz$b.weak.move.4, na.rm = TRUE)
        
        # undec_3rd_b_weak_move_5_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 471] <- xx[4, 2]
        df[k, 472] <- sd(zzz$b.weak.move.5, na.rm = TRUE)
        
        # undec_3rd_b_weak_move_6_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 473] <- xx[4, 2]
        df[k, 474] <- sd(zzz$b.weak.move.6, na.rm = TRUE)
        
        # undec_3rd_b_weak_move_7_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 475] <- xx[4, 2]
        df[k, 476] <- sd(zzz$b.weak.move.7, na.rm = TRUE)
        
        # undec_3rd_b_weak_move_8_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 477] <- xx[4, 2]
        df[k, 478] <- sd(zzz$b.weak.move.8, na.rm = TRUE)
        
        # undec_3rd_b_weak_move_9_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 479] <- xx[4, 2]
        df[k, 480] <- sd(zzz$b.weak.move.9, na.rm = TRUE)
        
        #####################
        ## Cumulative 0-50 ##
        #####################
        ## subset z - 3rd Qtr of game moves         
        #z75 <- as.integer(nrow(z) * 0.75)
        zzz <- zz[1:z50, ]
        
        ###################
        # undec_0-50_w_prev_score_median / mean / sd
        xx <- as.array(summary(zzz$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 481] <- xx[3, 2]
        df[k, 482] <- xx[4, 2]
        df[k, 483] <- sd(zzz$w.prev.score, na.rm = TRUE)
        
        # undec_0-50_w_delta_median / mean / sd
        xx <- as.array(summary(zzz$w.delta))
        xx <- as.data.frame(xx)
        df[k, 484] <- xx[3, 2]
        df[k, 485] <- xx[4, 2]
        df[k, 486] <- sd(zzz$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # undec_0-50_w_weak_move_1_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 487] <- xx[4, 2]
        df[k, 488] <- sd(zzz$w.delta, na.rm = TRUE)
        
        # undec_0-50_w_weak_move_2_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 489] <- xx[4, 2]
        df[k, 490] <- sd(zzz$w.weak.move.2, na.rm = TRUE )
        
        # undec_0-50_w_weak_move_3_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 491] <- xx[4, 2]
        df[k, 492] <- sd(zzz$w.weak.move.3, na.rm = TRUE)
        
        # undec_0-50_w_weak_move_4_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 493] <- xx[4, 2]
        df[k, 494] <- sd(zzz$w.weak.move.4, na.rm = TRUE)
        
        # undec_0-50_w_weak_move_5_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 495] <- xx[4, 2]
        df[k, 496] <- sd(zzz$w.weak.move.5, na.rm = TRUE)
        
        # undec_0-50_w_weak_move_6_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 497] <- xx[4, 2]
        df[k, 498] <- sd(zzz$w.weak.move.6, na.rm = TRUE)
        
        # undec_0-50_w_weak_move_7_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 499] <- xx[4, 2]
        df[k, 500] <- sd(zzz$w.weak.move.7, na.rm = TRUE)
        
        # undec_0-50_w_weak_move_8_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 501] <- xx[4, 2]
        df[k, 502] <- sd(zzz$w.weak.move.8, na.rm = TRUE)
        
        # undec_0-50_w_weak_move_9_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 503] <- xx[4, 2]
        df[k, 504] <- sd(zzz$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # undec_0-50_b_prev_score_median / mean / sd
        xx <- as.array(summary(zzz$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 505] <- xx[3, 2]
        df[k, 506] <- xx[4, 2]
        df[k, 507] <- sd(zzz$b.prev.score, na.rm = TRUE)
        
        # undec_0-50_b_delta_median / mean / sd
        xx <- as.array(summary(zzz$b.delta))
        xx <- as.data.frame(xx)
        df[k, 508] <- xx[3, 2]
        df[k, 509] <- xx[4, 2]
        df[k, 510] <- sd(zzz$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # undec_0-50_b_weak_move_1_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 511] <- xx[4, 2]
        df[k, 512] <- sd(zzz$b.weak.move.1, na.rm = TRUE)
        
        # undec_0-50_b_weak_move_2_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 513] <- xx[4, 2]
        df[k, 514] <- sd(zzz$b.weak.move.2, na.rm = TRUE)
        
        # undec_0-50_b_weak_move_3_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 515] <- xx[4, 2]
        df[k, 516] <- sd(zzz$b.weak.move.3, na.rm = TRUE)
        
        # undec_0-50_b_weak_move_4_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 517] <- xx[4, 2]
        df[k, 518] <- sd(zzz$b.weak.move.4, na.rm = TRUE)
        
        # undec_0-50_b_weak_move_5_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 519] <- xx[4, 2]
        df[k, 520] <- sd(zzz$b.weak.move.5, na.rm = TRUE)
        
        # undec_0-50_b_weak_move_6_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 521] <- xx[4, 2]
        df[k, 522] <- sd(zzz$b.weak.move.6, na.rm = TRUE)
        
        # undec_0-50_b_weak_move_7_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 523] <- xx[4, 2]
        df[k, 524] <- sd(zzz$b.weak.move.7, na.rm = TRUE)
        
        # undec_0-50_b_weak_move_8_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 525] <- xx[4, 2]
        df[k, 526] <- sd(zzz$b.weak.move.8, na.rm = TRUE)
        
        # undec_0-50_b_weak_move_9_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 527] <- xx[4, 2]
        df[k, 528] <- sd(zzz$b.weak.move.9, na.rm = TRUE)
        
        #####################
        #####################
        ## Cumulative 0-75 ##
        #####################
        ## subset z - 3rd Qtr of game moves         
        #z75 <- as.integer(nrow(z) * 0.75)
        zzz <- zz[1:z75, ]
        
        ###################
        # undec_0-75_w_prev_score_median / mean / sd
        xx <- as.array(summary(zzz$w.prev.score))
        xx <- as.data.frame(xx)
        df[k, 529] <- xx[3, 2]
        df[k, 530] <- xx[4, 2]
        df[k, 531] <- sd(zzz$w.prev.score, na.rm = TRUE)
        
        # undec_0-75_w_delta_median / mean / sd
        xx <- as.array(summary(zzz$w.delta))
        xx <- as.data.frame(xx)
        df[k, 532] <- xx[3, 2]
        df[k, 533] <- xx[4, 2]
        df[k, 534] <- sd(zzz$w.delta, na.rm = TRUE)
        
        ####################
        ## Calculate Mean of weak moves 1-9 for white
        # undec_0-75_w_weak_move_1_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 535] <- xx[4, 2]
        df[k, 536] <- sd(zzz$w.delta, na.rm = TRUE)
        
        # undec_0-75_w_weak_move_2_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 537] <- xx[4, 2]
        df[k, 538] <- sd(zzz$w.weak.move.2, na.rm = TRUE )
        
        # undec_0-75_w_weak_move_3_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 539] <- xx[4, 2]
        df[k, 540] <- sd(zzz$w.weak.move.3, na.rm = TRUE)
        
        # undec_0-75_w_weak_move_4_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 541] <- xx[4, 2]
        df[k, 542] <- sd(zzz$w.weak.move.4, na.rm = TRUE)
        
        # undec_0-75_w_weak_move_5_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 543] <- xx[4, 2]
        df[k, 544] <- sd(zzz$w.weak.move.5, na.rm = TRUE)
        
        # undec_0-75_w_weak_move_6_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 545] <- xx[4, 2]
        df[k, 546] <- sd(zzz$w.weak.move.6, na.rm = TRUE)
        
        # undec_0-75_w_weak_move_7_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 547] <- xx[4, 2]
        df[k, 548] <- sd(zzz$w.weak.move.7, na.rm = TRUE)
        
        # undec_0-75_w_weak_move_8_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 549] <- xx[4, 2]
        df[k, 550] <- sd(zzz$w.weak.move.8, na.rm = TRUE)
        
        # undec_0-75_w_weak_move_9_mean / sd
        xx <- as.array(summary(zzz$w.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 551] <- xx[4, 2]
        df[k, 552] <- sd(zzz$w.weak.move.9, na.rm = TRUE)
        
        ####################
        # undec_0-75_b_prev_score_median / mean / sd
        xx <- as.array(summary(zzz$b.prev.score))
        xx <- as.data.frame(xx)
        df[k, 553] <- xx[3, 2]
        df[k, 554] <- xx[4, 2]
        df[k, 555] <- sd(zzz$b.prev.score, na.rm = TRUE)
        
        # undec_0-75_b_delta_median / mean / sd
        xx <- as.array(summary(zzz$b.delta))
        xx <- as.data.frame(xx)
        df[k, 556] <- xx[3, 2]
        df[k, 557] <- xx[4, 2]
        df[k, 558] <- sd(zzz$b.delta, na.rm = TRUE)
        
        ## Calculate Mean of weak moves 1-9 for black
        # undec_0-75_b_weak_move_1_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.1))
        xx <- as.data.frame(xx)
        df[k, 559] <- xx[4, 2]
        df[k, 560] <- sd(zzz$b.weak.move.1, na.rm = TRUE)
        
        # undec_0-75_b_weak_move_2_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.2))
        xx <- as.data.frame(xx)
        df[k, 561] <- xx[4, 2]
        df[k, 562] <- sd(zzz$b.weak.move.2, na.rm = TRUE)
        
        # undec_0-75_b_weak_move_3_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.3))
        xx <- as.data.frame(xx)
        df[k, 563] <- xx[4, 2]
        df[k, 564] <- sd(zzz$b.weak.move.3, na.rm = TRUE)
        
        # undec_0-75_b_weak_move_4_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.4))
        xx <- as.data.frame(xx)
        df[k, 565] <- xx[4, 2]
        df[k, 566] <- sd(zzz$b.weak.move.4, na.rm = TRUE)
        
        # undec_0-75_b_weak_move_5_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.5))
        xx <- as.data.frame(xx)
        df[k, 567] <- xx[4, 2]
        df[k, 568] <- sd(zzz$b.weak.move.5, na.rm = TRUE)
        
        # undec_0-75_b_weak_move_6_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.6))
        xx <- as.data.frame(xx)
        df[k, 569] <- xx[4, 2]
        df[k, 570] <- sd(zzz$b.weak.move.6, na.rm = TRUE)
        
        # undec_0-75_b_weak_move_7_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.7))
        xx <- as.data.frame(xx)
        df[k, 571] <- xx[4, 2]
        df[k, 572] <- sd(zzz$b.weak.move.7, na.rm = TRUE)
        
        # undec_0-75_b_weak_move_8_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.8))
        xx <- as.data.frame(xx)
        df[k, 573] <- xx[4, 2]
        df[k, 574] <- sd(zzz$b.weak.move.8, na.rm = TRUE)
        
        # undec_0-75_b_weak_move_9_mean / sd
        xx <- as.array(summary(zzz$b.weak.move.9))
        xx <- as.data.frame(xx)
        df[k, 575] <- xx[4, 2]
        df[k, 576] <- sd(zzz$b.weak.move.9, na.rm = TRUE)
        
        #######################
        # Number of Rows
        #######################
        # nrow_game 
        df[k, 577] <- nrow(z)
        # nrow_undec_game
        df[k, 578] <- nrow(zz)
        
}


print(summary(df))
dim(df)
#df_back
dim(df_back)
#df[is.na(df)] <- 0

df[999:1002, ]

