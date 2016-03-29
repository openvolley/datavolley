## functions for dealing with the main data part of the datavolley file

skill_decode=function(skill,code) {
    if (!skill %in% c("S","R","A","B","D","E","F")) {
        stop("code: ",code," --- unexpected skill: ",skill)
    }
    switch(skill,
           S="Serve",
           R="Reception",
           A="Attack",
           B="Block",
           D="Dig",
           E="Set",
           F="Freeball",
           stop("Unknown skill: ",skill))
}

attack_map=function(type,skill) {
    switch(type,
           H=paste0("High ball ",skill),
           M=paste0("Half ball ",skill),
           Q=paste0("Quick ball ",skill),
           T=paste0("Head ball ",skill),
           U=paste0("Super ball ",skill),
           F=paste0("Fast ball ",skill),
           O=paste0("Other ",skill),
           paste0("Unknown ",skill," type"))
}

serve_map=function(type,skill) {
        switch(type,
               H=paste0("Float ",skill),
               M=paste0("Jump-float ",skill),
               Q=paste0("Jump ",skill),
               paste0("Unknown ",skill," type"))
    }
   
skill_type_decode=function(skill,type) {
    if (!skill %in% c("S","R","A","B","D","E","F")) {
        stop("unexpected skill: ",skill)
    }
    if (!type %in% c("H","M","Q","T","U","F","O")) {
        warning("skill ",skill," type: ",type," not recognized")
    }
    if (skill=="S") {
        serve_map(type,"serve")
    } else if (skill=="R") {
        serve_map(type,"serve reception")
    } else if (skill=="A") {
        attack_map(type,"attack")
    } else if (skill=="B") {
        attack_map(type,"block")
    } else if (skill=="D") {
        attack_map(type,"dig")
    } else if (skill=="E") {
        attack_map(type,"set")
    } else if (skill=="F") {
        "Unknown freeball type"
    } else {
        stop("Unknown skill: ",skill)
    }    
}
    
skill_eval_decode=function(skill,evaluation) {
    if (skill=="S") {
        switch(evaluation,
               "="="Error",
               "/"="Positive, no attack",
               "-"="Negative, opponent free attack",
               "+"="Positive, opponent some attack",
               "#"="Ace",
               "unknown serve evaluation")
    } else if (skill=="R") {
        switch(evaluation,
               "="="Error",
               "/"="Poor, no attack",
               "-"="Negative, limited attack",
               "+"="Positive, attack",
               "#"="Perfect pass",
               "unknown pass evaluation")
    } else if (skill=="A") {
        switch(evaluation,
               "="="Error",
               "/"="Blocked",
               "-"="Poor, easily dug",
               "!"="Blocked for reattack",
               "+"="Positive, good attack",
               "#"="Winning attack",
               "unknown attack evaluation")
    } else if (skill=="B") {
        switch(evaluation,
               "="="Error",
               "/"="Invasion",
               "-"="Poor, opposition to replay",
               "+"="Positive, block touch",
               "#"="Winning block",
               "unknown block evaluation")
    } else if (skill=="D") {
        switch(evaluation,
               "="="Error",
               "/"="Ball directly back over net",
               "-"="No structured attack possible",
               "#"="Good dig",
               "unknown dig evaluation")
    } else if (skill=="E") {
        switch(evaluation,
               "="="Error",
               "-"="Poor",
               "+"="Positive",
               "#"="Perfect",
               "unknown set evaluation")
    } else if (skill=="F") {
        switch(evaluation,
               "="="Error",
               "/"="Poor",
               "-"="OK, only high set possible",
               "+"="Good",
               "#"="Perfect",
               "unknown freeball evaluation")
    } else {
        stop("Unknown skill: ",skill)
    }
}

parse_code=function(code,meta) {
    out=data.frame(code=code,team=NA,player_number=NA,player_name=NA,skill=NA,skill_type=NA,evaluation=NA,
        attack_code=NA, attack_description=NA,
        set_code=NA, set_description=NA, set_type=NA,
        start_zone=NA,end_zone=NA,end_subzone=NA,
        skill_subtype=NA,num_players=NA,special_code=NA,
        timeout=FALSE,end_of_set=FALSE,substitution=FALSE,point=FALSE,home_team_score=NA,visiting_team_score=NA,stringsAsFactors=FALSE)
    for (ci in 1:nrow(out)) {
        code=out$code[ci]
        if (grepl("\\*\\*\\dset",code)) {
            ## end of set
            out$end_of_set[ci]=TRUE
        } else {
            team=substr(code,1,1)
            if (!team %in% c("a","*")) {
                stop("code does not start with * or a")
                team="unknown"
            }
            out$team[ci]=team
            if (substr(code,2,2) %in% c("z","p","P","C")) {
                ## automatic codes
                thischar=substr(code,2,2)
                if (thischar=="p") {
                    ## point for the associated team
                    out$point[ci]=TRUE
                    ## scores are given as .pX:Y where X=home team score, Y=visiting team score
                    temp=str_match(code,".p(\\d+):(\\d+)")
                    out$home_team_score[ci]=as.numeric(temp[2])
                    out$visiting_team_score[ci]=as.numeric(temp[3])
                } else if (thischar=="z") {
                    ## identifying the position of the setter
                } else if (thischar=="P") {
                    ## substitution of setter
                    out$substitution[ci]=TRUE
                } else if (thischar=="C") {
                    ## substitution of player
                    out$substitution[ci]=TRUE
                }
            } else if (substr(code,2,4)=="$$&") {
                ## win or loss of a point in an undefined way
                ## team marker here says which team played the ball
                ## but doesn't say which team won the point
                ## so don't set out$point here
            } else if (substr(code,2,4)=="$$R") {
                ## rotation error? not entirely sure, not many of these 
                ## these lines are followed by a $$& line, so don't do anything here for the time being
                out$skill[ci]="Rotation error"
                out$evaluation[ci]="Error"
            } else if (substr(code,2,4) %in% c("$$S","$$E")) {
                ## sanction
                ## not handled yet
                ## e.g.
                ##a$$SQ-;;;;;;;16.45.22;4;1;4;1;7138;;16;15;9;6;7;8;2;18;8;10;7;1;
                ##a$$EH=~~~~~~~~~RED;s;;;;;;16.45.22;4;1;4;1;7138;;16;15;9;6;7;8;2;18;8;10;7;1;
                
            } else if (substr(code,2,2)=="T") {
                ## timeout by this team
                out$timeout[ci]=TRUE
                out$skill[ci]="Timeout"
            } else if (substr(code,2,2)=="c") {
                ## substitution
                out$substitution[ci]=TRUE
            } else {
                player_number=str_match(code,".(\\d+)")
                player_number=as.numeric(player_number[2])
                if (is.na(player_number)) {
                    stop("unexpected player number in code: ",code)
                }
                player_name=get_player_name(team,player_number,meta)
                out$player_number[ci]=player_number
                out$player_name[ci]=player_name
                fullcode=code
                code=sub(".\\d+","",code)
                skill=substr(code,1,1)
                out$skill[ci]=skill_decode(skill,fullcode)
                hit_type=substr(code,2,2)
                out$skill_type[ci]=skill_type_decode(skill,hit_type)
                skill_eval=substr(code,3,3)
                out$evaluation[ci]=skill_eval_decode(skill,skill_eval)
                ## for attacks, next 2 chars are the attack code from the metadata$attacks table, and similarly for sets
                attack_code=substr(code,4,5)
                if (!attack_code %in% c("","~~")) {
                    if (skill=="A") {
                        out$attack_code[ci]=attack_code
                        if (!attack_code %in% meta$attacks$code) {
                            stop("unmatched attack code ",attack_code," in code ",fullcode)
                        }
                        out$attack_description[ci]=meta$attacks$description[meta$attacks$code==attack_code]
                    } else if (skill=="E") {
                        out$set_code[ci]=attack_code
                        if (!attack_code %in% meta$sets$code) {
                            stop("unmatched set code ",attack_code," in code ",fullcode)
                        }
                        out$set_description[ci]=meta$sets$description[meta$sets$code==attack_code]
                    } else {
                        warning("unexpected non-null attack code ",attack_code," for non-attack code ",fullcode)
                    }
                }
                set_type=substr(code,6,6)
                if (!set_type %in% c("","~")) {
                    if (skill=="E") {
                        out$set_type[ci]=set_type
                        if (!set_type %in% meta$attacks$set_type) {
                            stop("unmatched set type ",set_type," in code ",fullcode)
                        }
                    } else {
                        warning("found set type ",set_type," in code ",fullcode)
                    }
                }
                start_zone=substr(code,7,7)
                if (!start_zone %in% c("","~")) {
                    out$start_zone[ci]=as.numeric(start_zone)
                    if (skill %in% c("R","S") & !start_zone %in% c(1,5,6)) {
                        warning("serve/reception start zone ",start_zone," in code ",fullcode)
                    }
                }
                end_zone=substr(code,8,8)
                if (!end_zone %in% c("","~")) {
                    out$end_zone[ci]=as.numeric(end_zone)
                    if (skill %in% c("B") & !end_zone %in% c(2,3,4)) {
                        warning("block end zone ",end_zone," in code ",fullcode)
                    }
                }
                end_subzone=substr(code,9,9)
                if (!end_subzone %in% c("","~")) {
                    out$end_subzone[ci]=end_subzone
                    if (!end_subzone %in% c("A","B","C","D")) {
                        warning("end subzone ",end_subzone," in code ",fullcode)
                    }
                }
                ## skill sub type ("TYPE OF HIT", p32)
                skill_subtype=substr(code,10,10)
                if (!skill_subtype %in% c("","~")) {
                    if (skill=="A") {
                        if (!skill_subtype %in% c("H","P","T")) {
                            warning("unknown attack subtype ",skill_subtype," in code ",fullcode)
                        }
                        out$skill_subtype[ci]=switch(skill_subtype,
                            H="Hard spike",
                            P="Soft spike/topspin",
                            T="Tip",
                            paste0("Unknown ",skill_subtype))
                    } else if (skill=="B") {
                        if (!skill_subtype %in% c("A","T")) {
                            warning("unknown block subtype ",skill_subtype," in code ",fullcode)
                        }
                        out$skill_subtype[ci]=switch(skill_subtype,
                            A="Block assist",
                            T="Block attempt",
                            paste0("Unknown ",skill_subtype))
                    } else if (skill=="R") {
                        if (!skill_subtype %in% c("L","R","W","O","M")) {
                            warning("unknown reception subtype ",skill_subtype," in code ",fullcode)
                        }
                        out$skill_subtype[ci]=switch(skill_subtype,
                            L="On left",
                            R="On right",
                            W="Low",
                            O="Overhand",
                            M="Middle line",
                            paste0("Unknown ",skill_subtype))
                    } else if (skill=="D") {
                        if (!skill_subtype %in% c("S","C","B","E")) {
                            warning("unknown dig subtype ",skill_subtype," in code ",fullcode)
                        }
                        out$skill_subtype[ci]=switch(skill_subtype,
                            S="On spike",
                            C="Spike cover",
                            B="After block",
                            E="Emergency",
                            paste0("Unknown dig subtype ",skill_subtype))
                    } else {
                        out$skill_subtype[ci]=paste0("Unknown ",skill_subtype)
                    }
                }
                ## number of people ("PLAYERS", p33)
                num_players=substr(code,11,11)
                if (!num_players %in% c("","~")) {
                    if (skill=="A") {
                        if (!num_players %in% c("0","1","2","3")) {
                            warning("unexpected number of players ",num_players," in code ",fullcode)
                        }
                        out$num_players[ci]=switch(num_players,
                            "0"="No block",
                            "1"="1 player block",
                            "2"="2 player block",
                            "3"="3 player block",
                            paste0("Unexpected ",num_players))
                    } else if (skill=="B") {
                        if (!num_players %in% c("0","1","2","3","4")) {
                            warning("unexpected number of players ",num_players," in code ",fullcode)
                        }
                        out$num_players[ci]=switch(num_players,
                            "0"="No block",
                            "1"="1 player block",
                            "2"="2 player block",
                            "3"="3 player block",
                            "4"="Hole block",
                            paste0("Unexpected ",num_players))
                    } else if (skill=="R") {
                        if (!num_players %in% 1:9) {
                            warning("unexpected number of players ",num_players," in code ",fullcode)
                        }
                        out$num_players[ci]=switch(num_players,
                            "1"="Two players receiving, the player on left receives",
                            "2"="Two players receiving, the player on right receives",
                            "3"="Three players receiving, the player on left receives",
                            "4"="Three players receiving, the player on center receives",
                            "5"="Three players receiving, the player on light receives",
                            "6"="Four players receiving, the player on left receives",
                            "7"="Four players receiving, the player on center-left receives",
                            "8"="Four players receiving, the player on center-right receives",
                            "9"="Four players receiving, the player on right receives",
                            paste0("Unexpected ",num_players))
                    } else {
                        out$num_players[ci]=paste0("Unexpected ",num_players)
                    }
                }
                ## special ("SPECIAL CODES", p33)
                special_code=substr(code,12,12)
                if (!special_code %in% c("","~")) {
                    if (skill=="A") {
                        if (out$evaluation[ci]==skill_eval_decode("A","=")) {
                            ## error
                            if (!special_code %in% c("S","O","N","I","Z")) {
                                warning("unexpected special code ",special_code," in code ",fullcode)
                            }
                            out$special_code[ci]=switch(special_code,
                                "S"="Attack out - side",
                                "O"="Attack out - long",
                                "N"="Attack in net",
                                "I"="Net contact",
                                "Z"="Referee call",
                                paste0("Unexpected ",special_code))
                        } else if (out$evaluation[ci]==skill_eval_decode("A","#")) {
                            ## point ("Winning attack")
                            if (!special_code %in% c("S","O","F","X","N")) {
                                warning("unexpected special code ",special_code," in code ",fullcode)
                            }
                            out$special_code[ci]=switch(special_code,
                                "S"="Block out - side",
                                "O"="Block out - long",
                                "F"="Block on floor",
                                "X"="Direct on floor",
                                "N"="Let",
                                paste0("Unexpected ",special_code))
                        } else if (out$evaluation[ci] %in% c(skill_eval_decode("A","+"),skill_eval_decode("A","-"),skill_eval_decode("A","!"))) {
                            ## continue
                            if (!special_code %in% c("C","N")) {
                                warning("unexpected special code ",special_code," in code ",fullcode)
                            }
                            out$special_code[ci]=switch(special_code,
                                "C"="Block control",
                                "N"="Let",
                                paste0("Unexpected ",special_code))
                        } else {
                            ## not expecting special code for this attack evaluation outcome
                            warning("unexpected special code ",special_code," for attack evaluation \"",out$evaluation[ci],"\" in code ",fullcode)
                        }
                    } else if (skill=="B") {
                        if (!special_code %in% c("S","O","F","X","N","I","P","Z")) {
                            warning("unexpected special code ",special_code," in code ",fullcode)
                        }
                        out$special_code[ci]=switch(special_code,
                            "S"="Ball out - side",
                            "O"="Ball out - long",
                            "F"="Ball on floor",
                            "X"="Between hands",
                            "N"="Hands - net",
                            "I"="Net contact",
                            "P"="No jump",
                            "Z"="Referee call",
                            paste0("Unexpected ",special_code))
                    } else if (skill=="D") {
                        if (!special_code %in% c("U","X","P","Z")) {
                            warning("unexpected special code ",special_code," in code ",fullcode)
                        }
                        out$special_code[ci]=switch(special_code,
                            "U"="Unplayable",
                            "X"="Body error",
                            "P"="Position error",
                            "Z"="Referee call",
                            paste0("Unexpected ",special_code))
                    } else if (skill=="E") {
                        if (!special_code %in% c("U","I","Z")) {
                            warning("unexpected special code ",special_code," in code ",fullcode)
                        }
                        out$special_code[ci]=switch(special_code,
                            "U"="Cannot be hit",
                            "I"="Net touch",
                            "Z"="Referee call",
                            paste0("Unexpected ",special_code))
                    } else if (skill=="S") {
                        if (out$evaluation[ci]==skill_eval_decode("S","=")) {
                            ## error
                            if (!special_code %in% c("O","L","R","N")) {
                                warning("unexpected special code ",special_code," in code ",fullcode)
                            }
                            out$special_code[ci]=switch(special_code,
                                "O"="Ball out - long",
                                "L"="Ball out - left",
                                "R"="Ball out - right",
                                "N"="Ball in net",
                                paste0("Unexpected ",special_code))
                        } else if (out$evaluation[ci]==skill_eval_decode("S","#")) {
                            ## point (ace)
                            if (!special_code %in% c("N")) {
                                warning("unexpected special code ",special_code," in code ",fullcode)
                            }
                            out$special_code[ci]=switch(special_code,
                                "N"="Let",
                                paste0("Unexpected ",special_code))
                        } else if (out$evaluation[ci] %in% c(skill_eval_decode("S","/"),skill_eval_decode("S","-"),skill_eval_decode("S","+"))) {
                            ## continue
                            if (!special_code %in% c("N")) {
                                warning("unexpected special code ",special_code," in code ",fullcode)
                            }
                            out$special_code[ci]=switch(special_code,
                                "N"="Let",
                                paste0("Unexpected ",special_code))
                        } else {
                            ## not expecting special code for this attack evaluation outcome
                            warning("unexpected special code ",special_code," for attack evaluation \"",out$evaluation[ci],"\" in code ",fullcode)
                        }
                    }
                }
                if (nchar(code)>12) {
                    warning("code: ",fullcode," new info ",substr(code,13,nchar(code)))
                }
            }
        }
    }
    out
}
    
read_main=function(filename) {
    x=data.table::fread(filename,skip="[3SCOUT]",data.table=FALSE)
    names(x)[1]="code"
    names(x)[8]="time"
    ##x$time=hms(x$time)
    names(x)[13]="video_time"
    names(x)[15:20]=paste("home_p",1:6,sep="") ## home team, court positons 1-6, entries are player numbers
    names(x)[21:26]=paste("visiting_p",1:6,sep="") ## visiting team
    x$code=as.character(x$code)
    x
}
