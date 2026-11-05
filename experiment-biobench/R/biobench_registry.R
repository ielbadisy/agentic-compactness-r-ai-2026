# Copied verbatim from project-biobench/R/00-registry.R (audited 2026-09-20).
# Only the classification and regression entries are used here.

# Dataset registry for the biobench benchmark.
# One entry per dataset: task, outcome definition, leak/identifier columns to drop,
# grouping column for repeated measures. Every dataset-specific decision lives here.
# `status` (final values are set by the evidence table at the end of this file):
#   documented          the help page of the source package (or of biobench) states the outcome and its coding
#   outcome_documented  the help page names the outcome column(s) but not the coding; the coding was checked
#                       against the data
#   inferred            neither (no dataset is in this class at present)
# `evidence` says which help page supports the entry and what was decided by the registry.

spec <- function(task, y = NULL, time = NULL, event = NULL, event_is = 1,
                 drop = NULL, keep = NULL, group = NULL, transform = NULL,
                 status = "inferred", note = "", source = NULL) {
  # `source`: name of the biobench dataset when this entry is an additional analysis of it
  list(task = task, y = y, time = time, event = event, event_is = event_is,
       drop = drop, keep = keep, group = group, transform = transform,
       status = status, note = note, source = source, evidence = "")
}

# Expand aggregated case-control counts into one row per subject.
expand_counts <- function(d) {
  cases <- d[rep(seq_len(nrow(d)), d$ncases), setdiff(names(d), c("ncases", "ncontrols")), drop = FALSE]
  ctrls <- d[rep(seq_len(nrow(d)), d$ncontrols), setdiff(names(d), c("ncases", "ncontrols")), drop = FALSE]
  if (nrow(cases)) cases$case <- "yes"
  if (nrow(ctrls)) ctrls$case <- "no"
  out <- rbind(cases, ctrls)
  out$case <- factor(out$case, levels = c("no", "yes"))
  rownames(out) <- NULL
  out
}

registry <- list(
  # ---- classification (22) ----
  breastcancer   = spec("classification", y = "Class", drop = "Id", status = "documented"),
  pima2          = spec("classification", y = "diabetes", status = "documented"),
  dna            = spec("classification", y = "Class", status = "documented"),
  covidtest      = spec("classification", y = "result",
                        drop = c("subject_id", "fake_first_name", "fake_last_name", "test_id",
                                 "ct_result", "col_rec_tat", "rec_ver_tat"),
                        transform = function(d) { d <- d[d$result %in% c("positive", "negative"), ]; d },
                        note = "positive vs negative; invalid results removed; ct_result and turnaround times dropped (post-result)"),
  esophca        = spec("classification", y = "case", transform = expand_counts,
                        note = "aggregated counts expanded to one row per subject (case vs control)"),
  indorct        = spec("classification", y = "outcome", drop = c("id", "site"),
                        note = "post-ERCP outcome"),
  licorice       = spec("classification", y = "throat_pain_30min",
                        drop = c(grep("^(pacu|postOp|pod1)", names(biobench::licorice), value = TRUE), "extubation_cough"),
                        transform = function(d) { d$throat_pain_30min <- factor(ifelse(d$pacu30min_throatPain > 0, "yes", "no"), levels = c("no", "yes")); d },
                        note = "sore throat at 30 minutes (any); post-operative columns dropped; confirm against Ruetzler 2013"),
  opt            = spec("classification", y = "Preg.ended...37.wk",
                        keep = local({ nm <- names(biobench::opt)
                                       unique(c(nm[2:28], nm[37:48], grep("^BL", nm, value = TRUE))) }),
                        transform = function(d) {
                          d <- d[trimws(as.character(d$Birth.outcome)) %in% c("Live birth", "Non-live birth"), ]
                          d[["Preg.ended...37.wk"]] <- factor(trimws(as.character(d[["Preg.ended...37.wk"]])), levels = c("No", "Yes")); d },
                        note = "documented column: pregnancy ended before 37 weeks; elective abortions and lost to follow-up removed; baseline predictors only"),
  scurvy         = spec("classification", y = "fit_for_duty_d6", drop = "study_id",
                        note = "n = 12; other day-6 symptoms dropped as post-outcome",
                        transform = function(d) { d[, c("treatment", "dosing_regimen_for_scurvy", "fit_for_duty_d6")] }),
  streptb        = spec("classification", y = "improved", drop = c("patient_id", "radiologic_6m", "rad_num")),
  haberman       = spec("classification", y = "survival_status"),
  highriskpreg   = spec("classification", y = "risk_level", drop = "study_id",
                        transform = function(d) { d <- d[!is.na(d$risk_level), ]; d$risk_level <- droplevels(d$risk_level); d },
                        note = "empty risk level removed"),
  tobaccomorocco = spec("classification", y = "cigar_use", drop = c("FinalWgt", "Stratum", "PSU"), status = "documented",
                        transform = function(d) d[d$cigar_use %in% c("No", "Yes"), ],
                        note = "survey weight and design variables dropped; empty outcome values removed"),
  respiratory    = spec("classification", y = "status", group = "subject"),
  toenail        = spec("classification", y = "outcome", group = "patientID"),
  icu            = spec("classification", y = "sta", drop = "id"),
  glow500        = spec("classification", y = "fracture", drop = c("sub_id", "site_id", "phy_id")),
  myopia         = spec("classification", y = "myopic", drop = "id"),
  nhanes         = spec("classification", y = "obese", drop = c("id", "samplewt", "psu", "strata", "bmi", "wt", "ht"),
                        note = "bmi, weight, height dropped: they define obesity"),
  polypharm      = spec("classification", y = "polypharmacy", drop = "id"),
  chdage         = spec("classification", y = "chd", drop = c("id", "agegrp")),
  burn1000       = spec("classification", y = "death", drop = "id"),

  # ---- regression (10) ----
  indometh       = spec("regression", y = "conc", group = "Subject"),
  laryngoscope   = spec("regression", y = "total_intubation_time",
                        keep = c("age", "gender", "asa", "BMI", "Mallampati", "Randomization"),
                        note = "baseline predictors only; attempt-level columns are post-randomization"),
  polyps         = spec("regression", y = "number12m", drop = "participant_id"),
  smartpill      = spec("regression", y = "WG.Time", drop = c("GE.Time", "SB.Time", "C.Time"),
                        note = "whole-gut transit time; component transit times dropped"),
  theoph         = spec("regression", y = "conc", group = "Subject"),
  birthwt        = spec("regression", y = "bwt", drop = "low", status = "documented", note = "low is derived from bwt"),
  anorexia       = spec("regression", y = "Postwt"),
  aidsdelay      = spec("regression", y = "y"),
  agefat         = spec("regression", y = "fat"),
  phosphate      = spec("regression", y = "t5", note = "last measurement predicted from group and earlier measurements"),

  # ---- survival (31) ----
  bloodstorage   = spec("survival", time = "TimeToRecurrence", event = "Recurrence", drop = "Censor", status = "documented"),
  cmv            = spec("survival", time = "time.to.cmv", event = "cmv",
                        drop = c("ID", "agvhd", "time.to.agvhd", "cgvhd", "time.to.cgvhd"),
                        note = "post-transplant events dropped"),
  supraclav      = spec("survival", time = "med_duration", event = "med_censor", event_is = 0,
                        drop = c("subject", "onset_sensory", "onset_first_sensory", "onset_motor", "nerve_block_censor",
                                 "vps_rest", "vps_movement", "opioid_total"),
                        note = "med_censor = 1 marks censoring (all such durations equal 48, the administrative cutoff), so event is med_censor = 0"),
  lung           = spec("survival", time = "time", event = "status", event_is = 2, drop = "inst"),
  veteran        = spec("survival", time = "time", event = "status"),
  pbc            = spec("survival", time = "time", event = "status", event_is = 2, drop = "id",
                        note = "death; transplant treated as censored"),
  colon          = spec("survival", time = "time", event = "status", drop = c("id", "etype"),
                        transform = function(d) d[d$etype == 2, ], note = "death (etype 2), one row per patient"),
  flchain        = spec("survival", time = "futime", event = "death", drop = "chapter",
                        note = "cause-of-death chapter dropped (post-outcome)"),
  rotterdam      = spec("survival", time = "dtime", event = "death", drop = c("pid", "rtime", "recur"),
                        note = "overall survival; recurrence columns dropped"),
  gbsg2          = spec("survival", time = "time", event = "cens"),
  crcfes         = spec("survival", time = "time", event = "event"),
  crcmondaca     = spec("survival", time = "time", event = "status", drop = "patient_id"),
  framingham     = spec("survival", time = "time", event = "status", drop = c("cause", "cexam", "chd", "cva", "ca", "oth"),
                        note = "cause and event-type columns dropped (post-outcome)"),
  heartfailure   = spec("survival", time = "time", event = "DEATH_EVENT"),
  metabric       = spec("survival", time = "overall_survival_months", event = "overall_survival",
                        event_is = 0, drop = c("patient_id", "death_from_cancer"),
                        transform = function(d) {
                          mc <- grep("_mut$", names(d), value = TRUE)
                          for (v in mc) d[[v]] <- factor(ifelse(as.character(d[[v]]) == "0", "no", "yes"), levels = c("no", "yes"))
                          d },
                        note = "overall_survival is coded 1 = living, so event is 0 (matches death_from_cancer); mutation strings binarized to mutated yes/no"),
  bmt            = spec("survival", time = "t2", event = "d3",
                        drop = c("t1", "d1", "d2", "ta", "da", "tc", "dc", "tp", "dp"),
                        note = "disease-free survival; post-transplant event columns dropped"),
  drug6mp        = spec("survival", time = "t2", event = "relapse", drop = "pair", note = "n = 21 paired design"),
  larynx         = spec("survival", time = "time", event = "delta"),
  kidney         = spec("survival", time = "time", event = "delta"),
  std            = spec("survival", time = "time", event = "rinfct", drop = "obs"),
  channing       = spec("survival", time = "time", event = "death", drop = c("obs", "age"),
                        note = "left truncation ignored; exit age dropped (entry age plus time)"),
  mitrace        = spec("survival", time = "time", event = "status", event_is = 9, drop = "id",
                        note = "cause-specific; other causes censored; code 9 to confirm"),
  stroke         = spec("survival", time = "time", event = "status"),
  pbc3           = spec("survival", time = "days", event = "status", event_is = c(1, 2), drop = "ptno",
                        note = "death or transplant composite; codes to confirm"),
  melanoma       = spec("survival", time = "time", event = "event", event_is = "death.malignant.melanoma",
                        drop = c("status", "logthick"), note = "cause-specific; other deaths censored"),
  paquid         = spec("survival", time = "time", event = "status", note = "dementia (status 1); death without dementia censored; codes to confirm"),
  support        = spec("survival", time = "d.time", event = "death", drop = "slos"),
  erspc          = spec("survival", time = "Follow.Up.Time", event = "DeadOfPrCa"),
  bmtcrr         = spec("survival", time = "ftime", event = "Status", note = "cause-specific; event of interest is Status 1, competing event censored"),
  eprchd         = spec("survival", time = "time", event = "status"),
  nwtsco         = spec("survival", time = "trel", event = "relaps", drop = c("tsur", "dead"),
                        note = "relapse-free time; survival columns dropped"),

  # ---- complementary cause-specific analyses (the source documentation defines a second cause) ----
  bmtcrr_cause2  = spec("survival", time = "ftime", event = "Status", event_is = 2, source = "bmtcrr",
                        note = "competing event (Status 2); relapse (Status 1) is censored here"),
  melanoma_other = spec("survival", time = "time", event = "event", event_is = "death.other.causes", source = "melanoma",
                        drop = c("status", "logthick"), note = "death from other causes; melanoma death censored"),
  paquid_death   = spec("survival", time = "time", event = "status", event_is = 2, source = "paquid",
                        note = "death without dementia; dementia onset (status 1) censored"),
  mitrace_other  = spec("survival", time = "time", event = "status", event_is = c(7, 8), drop = "id", source = "mitrace",
                        note = "deaths from other causes (7, 8); death from myocardial infarction (9) censored")
)

# ---- evidence for every entry: status and the source that supports it -----------------------------
# Written 2026-09-20 from the help pages of the source packages (utils::help()) and of biobench.
.ev <- function(status, text) list(status = status, text = text)
registry_evidence <- list(
  # classification
  breastcancer   = .ev("documented", "biobench (mlbench): Class, benign or malignant; Id dropped"),
  pima2          = .ev("documented", "biobench (mlbench): diabetes, neg or pos"),
  dna            = .ev("documented", "biobench (mlbench): Class, ei, ie, or n"),
  covidtest      = .ev("documented", "medicaldata::covid_testing: result (positive, negative, invalid). Registry: invalid removed; ct_result and turnaround times dropped as post-result"),
  esophca        = .ev("documented", "medicaldata::esoph_ca: aggregated case-control counts (ncases, ncontrols). Registry: expanded to one row per subject"),
  indorct        = .ev("documented", "medicaldata::indo_rct: outcome, post-ERCP pancreatitis (0_no, 1_yes)"),
  licorice       = .ev("documented", "medicaldata::licorice_gargle: pacu30min_throatPain, sore throat score at 30 minutes in PACU. Registry: binarized as any pain"),
  opt            = .ev("documented", "medicaldata::opt: Preg.ended...37.wk, whether the pregnancy ended before 37 weeks (equals gestational age under 259 days in all 101 live and non-live births)"),
  scurvy         = .ev("documented", "medicaldata::scurvy: fit_for_duty_d6, dichotomous fitness for duty. Registry: other day-6 symptoms dropped as post-outcome"),
  streptb        = .ev("documented", "medicaldata::strep_tb: improved, defined as radiologic rating 5 to 6 (rad_num); rad_num and radiologic_6m dropped"),
  haberman       = .ev("outcome_documented", "biostatlab::haberman: outcome survival_status named; level coding not documented"),
  highriskpreg   = .ev("outcome_documented", "biostatlab::high_risk_pregnancy: outcome Risk Level named (risk_level in biobench >= 0.2.0). Registry: rows with a missing level removed"),
  tobaccomorocco = .ev("documented", "biobench (biostatlab): primary outcome cigar_use; survey weight and design columns dropped"),
  respiratory    = .ev("documented", "HSAUR3::respiratory: status is the response (poor, good); four monthly visits, resampled by subject"),
  toenail        = .ev("documented", "HSAUR3::toenail: outcome is the degree of separation of the nail plate; repeated visits, resampled by patientID"),
  icu            = .ev("documented", "aplore3::icu: sta, vital status at hospital discharge (Lived, Died)"),
  glow500        = .ev("documented", "aplore3::glow500: fracture, any fracture in the first year"),
  myopia         = .ev("documented", "aplore3::myopia: myopic, myopia within the first five years"),
  nhanes         = .ev("documented", "aplore3::nhanes: obese is BMI above 35, so bmi, wt, and ht are dropped"),
  polypharm      = .ev("documented", "aplore3::polypharm: polypharmacy, marked Outcome (drugs from more than three classes)"),
  chdage         = .ev("documented", "aplore3::chdage: chd, presence of coronary heart disease; agegrp is derived from age and dropped"),
  burn1000       = .ev("documented", "aplore3::burn1000: death, hospital discharge status"),
  # regression
  indometh       = .ev("documented", "medicaldata::indometh: conc, plasma concentration; repeated measures, resampled by Subject"),
  laryngoscope   = .ev("documented", "medicaldata::laryngoscope: total_intubation_time in seconds. Registry: baseline predictors only"),
  polyps         = .ev("documented", "medicaldata::polyps: number12m, number of polyps at 12 months"),
  smartpill      = .ev("documented", "medicaldata::smartpill: WG.Time, whole gut transit time in hours. Registry: component transit times dropped"),
  theoph         = .ev("documented", "medicaldata::theoph: conc, plasma concentration; repeated measures, resampled by Subject"),
  birthwt        = .ev("documented", "biobench (MASS): bwt, birth weight; low is derived from it"),
  anorexia       = .ev("documented", "MASS::anorexia: Postwt, weight after the study period"),
  aidsdelay      = .ev("documented", "boot::aids: y, the number of AIDS cases reported"),
  agefat         = .ev("documented", "HSAUR3::agefat: fat, body fat percentage"),
  phosphate      = .ev("documented", "HSAUR3::phosphate: t5, phosphate level after five hours. Registry: predicted from group and earlier measurements"),
  # survival
  bloodstorage   = .ev("documented", "biobench (medicaldata): Recurrence and TimeToRecurrence; Censor is the complement"),
  cmv            = .ev("documented", "medicaldata::cytomegalovirus: cmv (reactivation, 0 or 1) and time.to.cmv in months. Registry: other post-transplant events dropped"),
  supraclav      = .ev("documented", "medicaldata::supraclavicular: med_duration in hours and med_censor (patients without analgesic censored at 48 hours), so the event is med_censor 0. Registry: onset and block-failure columns dropped"),
  lung           = .ev("documented", "survival::lung: status 1 = censored, 2 = dead"),
  veteran        = .ev("outcome_documented", "survival::veteran: time and status (censoring status) named; coding 1 = death checked against the data (128 of 137)"),
  pbc            = .ev("documented", "survival::pbc: status 0, 1, 2 = censored, transplant, dead. Registry: event is death, transplant censored"),
  colon          = .ev("outcome_documented", "survival::colon: etype 1 = recurrence, 2 = death documented; status is the censoring status, coding checked against the data. Registry: death rows"),
  flchain        = .ev("documented", "survival::flchain: futime and death (0 alive, 1 dead); chapter (cause of death) dropped"),
  rotterdam      = .ev("documented", "survival::rotterdam: dtime and death (0 alive, 1 dead). Registry: recurrence columns dropped"),
  gbsg2          = .ev("documented", "TH.data::GBSG2: time and cens (0 censored, 1 event)"),
  crcfes         = .ev("outcome_documented", "biostatlab::crc_fes: outcome columns time and event named; coding not documented"),
  crcmondaca     = .ev("outcome_documented", "biostatlab::crc_mondaca2020: outcome columns time and status named; coding not documented"),
  framingham     = .ev("outcome_documented", "biostatlab::framingham: outcome columns time and status named. Registry: cause and event-type columns dropped"),
  heartfailure   = .ev("outcome_documented", "biostatlab::heart_failure: outcome columns time and DEATH_EVENT named; coding not documented"),
  metabric       = .ev("outcome_documented", "biostatlab::metabric: overall_survival_months and overall_survival named; coding checked against death_from_cancer (1 = living, so the event is 0)"),
  bmt            = .ev("documented", "KMsurv::bmt: t2 and d3, disease-free survival time and indicator (1 = dead or relapsed)"),
  drug6mp        = .ev("documented", "KMsurv::drug6mp: t2, time to relapse for 6-MP patients, and relapse (0 censored, 1 relapse)"),
  larynx         = .ev("documented", "KMsurv::larynx: time and delta (death indicator)"),
  kidney         = .ev("documented", "KMsurv::kidney: time to infection and delta (infection indicator)"),
  std            = .ev("documented", "KMsurv::std: time to reinfection and rinfct (reinfection)"),
  channing       = .ev("documented", "KMsurv::channing: death status and time (difference between exit age and entry age); exit age dropped. Left truncation ignored"),
  mitrace        = .ev("documented", "timereg::TRACE: status 9 = dead from myocardial infarction, 0 = alive, 7 and 8 = dead from other causes (censored here)"),
  stroke         = .ev("documented", "pec::cost (Copenhagen Stroke Study, 518 patients): time in days, status 0 = censored, 1 = event"),
  pbc3           = .ev("documented", "pec::Pbc3: main response is failure of medical treatment, death or liver transplantation (status 1 or 2)"),
  melanoma       = .ev("documented", "riskRegression::Melanoma: event is censored, death.malignant.melanoma, or death.other.causes (other deaths censored here)"),
  paquid         = .ev("documented", "riskRegression::Paquid: status 1 = dementia onset, 2 = death without dementia (censored here)"),
  support        = .ev("documented", "casebase::support: death and d.time, follow-up to death. Registry: slos (hospital stay) dropped"),
  erspc          = .ev("documented", "casebase::ERSPC: DeadOfPrCa 1 = death from prostate cancer, 0 = other causes or administrative; time in years"),
  bmtcrr         = .ev("documented", "casebase::bmtcrr: Status 0 = censored, 1 = relapse, 2 = competing event; ftime in months"),
  eprchd         = .ev("documented", "casebase::eprchd: status 0 = censored, 1 = event; time in years"),
  nwtsco         = .ev("documented", "addhazard::nwtsco: trel and relaps (relapse); tsur and dead (death) dropped"),
  # complementary cause-specific analyses
  bmtcrr_cause2  = .ev("documented", "casebase::bmtcrr: Status 2 = competing event"),
  melanoma_other = .ev("documented", "riskRegression::Melanoma: death.other.causes"),
  paquid_death   = .ev("documented", "riskRegression::Paquid: status 2 = death without dementia"),
  mitrace_other  = .ev("documented", "timereg::TRACE: status 7 and 8 = dead from other causes")
)
stopifnot(setequal(names(registry), names(registry_evidence)))
for (.nm in names(registry)) {
  registry[[.nm]]$status <- registry_evidence[[.nm]]$status
  registry[[.nm]]$evidence <- registry_evidence[[.nm]]$text
}
rm(.nm)
