# 1. Cleaning, Merging, and Exploring Qualitative Data

Qualitative projects rarely begin with a dataset that is ready for
analysis and raw Dedoose exports do not include a dedicated ID column.
Instead, they arrive with transcript metadata, coder names, ranges,
weights, and one column per code. This vignette walks through a typical
workflow—cleaning excerpts, merging related codes, and reviewing the
underlying text—using three core functions:
[`clean_data()`](https://abiraahmi.github.io/DedooseR/reference/clean_data.md),
[`recode_themes()`](https://abiraahmi.github.io/DedooseR/reference/recode_themes.md),
and
[`view_excerpts()`](https://abiraahmi.github.io/DedooseR/reference/view_excerpts.md).
To keep the example fun, we follow a fictional qualitative study of a
summer music camp exploring how drumming circles and songwriting labs
support child development. The mock tibble below mirrors a Dedoose
export so the entire vignette can render without private files.

``` r

library(DedooseR)
library(tibble)
library(purrr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union


# Mock excerpts that mimic a Dedoose export ----------------------------------
code_order <- c(
  "Motor Skills",
  "Social Skills",
  "Self-expression",
  "Emotional Regulation",
  "Emotion Identification",
  "Inhibitory Control",
  "Internalizing Behavior",
  "Externalizing Behavior",
  "Academic Skills",
  "Self-esteem",
  "Growth Mindset"
)

code_weights <- c(
  "Motor Skills" = 1,
  "Social Skills" = 1,
  "Self-expression" = 1,
  "Emotional Regulation" = 2,
  "Emotion Identification" = 2,
  "Inhibitory Control" = 1,
  "Internalizing Behavior" = 2,
  "Externalizing Behavior" = 1,
  "Academic Skills" = 1,
  "Self-esteem" = 1,
  "Growth Mindset" = 1
)

code_assignments <- list(
  c("Motor Skills", "Social Skills", "Self-expression", "Academic Skills", "Self-esteem", "Growth Mindset"),
  c("Social Skills", "Emotional Regulation", "Emotion Identification", "Inhibitory Control", "Externalizing Behavior", "Academic Skills", "Growth Mindset"),
  c("Self-expression", "Emotional Regulation", "Emotion Identification", "Internalizing Behavior", "Academic Skills", "Self-esteem"),
  c("Social Skills", "Self-expression", "Emotion Identification", "Inhibitory Control", "Academic Skills", "Self-esteem", "Growth Mindset"),
  c("Emotional Regulation", "Emotion Identification", "Internalizing Behavior", "Externalizing Behavior", "Academic Skills", "Self-esteem"),
  c("Motor Skills", "Emotional Regulation", "Inhibitory Control", "Internalizing Behavior", "Academic Skills", "Growth Mindset"),
  c("Social Skills", "Self-expression", "Externalizing Behavior", "Academic Skills", "Self-esteem"),
  c("Motor Skills", "Social Skills", "Emotional Regulation", "Inhibitory Control", "Academic Skills", "Self-esteem", "Growth Mindset"),
  c("Self-expression", "Emotional Regulation", "Emotion Identification", "Inhibitory Control", "Internalizing Behavior", "Academic Skills", "Self-esteem", "Growth Mindset"),
  c("Social Skills", "Emotional Regulation", "Internalizing Behavior", "Externalizing Behavior", "Academic Skills", "Self-esteem"),
  c("Self-expression", "Emotion Identification", "Inhibitory Control", "Internalizing Behavior", "Academic Skills", "Growth Mindset"),
  c("Social Skills", "Emotional Regulation", "Internalizing Behavior", "Academic Skills", "Self-esteem"),
  c("Motor Skills", "Self-expression", "Inhibitory Control", "Academic Skills", "Self-esteem", "Growth Mindset"),
  c("Emotional Regulation", "Emotion Identification", "Externalizing Behavior", "Academic Skills", "Self-esteem"),
  c("Social Skills", "Internalizing Behavior", "Academic Skills", "Growth Mindset"),
  c("Motor Skills", "Emotional Regulation", "Inhibitory Control", "Internalizing Behavior", "Academic Skills", "Self-esteem"),
  c("Self-expression", "Emotion Identification", "Internalizing Behavior", "Academic Skills", "Self-esteem", "Growth Mindset"),
  c("Social Skills", "Emotional Regulation", "Inhibitory Control", "Externalizing Behavior", "Academic Skills", "Self-esteem"),
  c("Self-expression", "Internalizing Behavior", "Academic Skills", "Growth Mindset"),
  c("Social Skills", "Emotional Regulation", "Emotion Identification", "Inhibitory Control", "Internalizing Behavior", "Academic Skills", "Self-esteem", "Growth Mindset")
)

demo_excerpts <- tibble(
  `Media Title` = c(
    "Interview: Opening Circle",
    "Focus Group: Rhythm Relay",
    "Interview: Alumni Showcase",
    "Classroom Observation: Lyric Lab",
    "Journal Club: Campfire Reflections",
    "Parent Interview: Practice Night",
    "Coach Debrief: Studio Session",
    "Interview: Teen Beat Makers",
    "Workshop: Beat Mapping",
    "Focus Group: Mentor Circle",
    "Observation: Homework Jam",
    "Interview: Closing Showcase",
    "Survey Comment: Quiet Camper",
    "Survey Comment: Confident Soloist",
    "Interview: Leadership Huddle",
    "Observation: Peer Pairing",
    "Focus Group: Reflection Rounds",
    "Interview: New Camper",
    "Observation: Goal Setting",
    "Workshop: Improvisation Lab"
  ),
  `Excerpt Range` = c(
    "Excerpt 12-56",
    "Excerpt 04-38",
    "Excerpt 77-112",
    "Excerpt 15-42",
    "Excerpt 33-80",
    "Excerpt 08-29",
    "Excerpt 21-48",
    "Excerpt 44-78",
    "Excerpt 63-97",
    "Excerpt 10-35",
    "Excerpt 52-85",
    "Excerpt 18-41",
    "Excerpt 71-103",
    "Excerpt 26-50",
    "Excerpt 39-72",
    "Excerpt 14-36",
    "Excerpt 58-90",
    "Excerpt 23-47",
    "Excerpt 66-101",
    "Excerpt 30-63"
  ),
  `Excerpt Creator` = c(
    "Aliyah",
    "Rohan",
    "Aliyah",
    "Lina",
    "Mateo",
    "Priya",
    "Lina",
    "Rohan",
    "Aliyah",
    "Mateo",
    "Priya",
    "Jordan",
    "Aliyah",
    "Rohan",
    "Jordan",
    "Priya",
    "Lina",
    "Mateo",
    "Jordan",
    "Priya"
  ),
  `Excerpt Date` = as.Date(c(
    "2023-06-10",
    "2023-07-18",
    "2024-01-08",
    "2023-07-24",
    "2023-08-05",
    "2023-08-12",
    "2023-08-19",
    "2023-08-26",
    "2023-09-02",
    "2023-09-09",
    "2023-09-16",
    "2023-09-23",
    "2023-09-30",
    "2023-10-07",
    "2023-10-14",
    "2023-10-21",
    "2023-10-28",
    "2023-11-04",
    "2023-11-11",
    "2023-11-18"
  )),
  `Excerpt Copy` = c(
    "Keeping the djembe rhythm steady made my hands stronger, and pairing up for call-and-response helped the nervous kids giggle and join in. We counted the measures together like a quick math warmup, and they kept testing new patterns until they could hear their confidence rise.",
    "During the rhythm relay, we practiced breathing between verses so the kids could wait for their cue instead of jumping in early. Mapping the rhymes to eight-count phrases let them chart syllables like a fractions lesson, and when one camper shouted over the group we paused to name the frustration, reset, and talk about how mistakes fuel the next take.",
    "When I get mad now, I write a hook about it first. It helps me spot the feeling, note the anxious thoughts that used to stay bottled up, and decide whether I want to shout it or sing it soft. Drafting verses in my notebook also keeps my reading teacher happy because I'm practicing structure while I hype myself up.",
    "In lyric lab the trio revised their chorus, taking turns so nobody rushed the mic while they jotted rhyme schemes on the whiteboard. Naming which lines sounded brave versus nervous let them cheer each other on, and the rewrites doubled as a grammar check that made them proud to keep refining.",
    "During the campfire reflection, one camper admitted that she sometimes shuts down when we practice sad songs. Now, we check in during practice to spot evidence for those feelings and share coping plans when needed",
    "On practice night we tapped the rim pattern slowly so the shy drummer could breathe through the butterflies and wait for the downbeat before attacking the snare. They tracked tempos in a spreadsheet for homework, proving to themselves that patient repetitions improve control even when anxiety creeps in mid-song.",
    "During the studio session debrief, the counselors noted how Tasha's freestyles shifted from loud interruptions to collaborative echoes once we rehearsed conversation stems. She marked the beats she wanted to emphasize on her lyric worksheet, and bragged that the script gave her confidence without shutting down her big energy.",
    "The teen beat makers synced their keyboard riffs with the drum pad, counting rests together so each person held back until the loop called for them. Charting the chord inversions on staff paper turned into a mini theory lesson, and the crew loved seeing how breathing through the tricky hand switch built real confidence.",
    "Aliyah's notebook shows mood colors beside every verse, and she journals how the sadness feels in her chest before choosing whether to rap or sing the line. She keeps a checklist of revision strategies from literacy class, reminding herself to pause, name the feeling, and try another take until the lyric matches her intention.",
    "In mentor circle today the group mapped conflict scripts, role-playing the kid who shuts down and the kid who pops off. They located sentence starters from our reading packet to practice calm responses, and sharing wins boosted the quieter campers' confidence.",
    "During homework jam we slowed spoken word drafts, pausing at commas to note the emotions driving each line. Writing margin notes about the worry behind certain phrases helped them see how breathing before the final stanza keeps the story clear while they revise.",
    "At the closing showcase interview, Jordan described how the ensemble now checks in with thumbs before feedback so anxious singers feel seen. They referenced the critique rubric like a reading comprehension guide, and noticing their own growth made them stand taller on stage.",
    "While recording harmonies Aliyah plotted fingerings on a tablature grid and waited for the metronome click before sliding into the chorus. Tracking the takes in her practice journal showed how deliberate pacing builds confidence and keeps her chasing new techniques.",
    "Rohan noted that the lunchtime cypher got loud when Jace joked through the tension, so we paused to label the frustration and mark triggers on the behavior chart. Translating that moment into a reflective paragraph later helped Jace feel proud of naming the shift before it exploded again.",
    "In leadership huddle the mentors graphed how their small-group questions improved week to week, even when nerves made them rush. Studying discussion transcripts for evidence of follow-up prompts showed them that practicing new openings pays off.",
    "During peer pairing, Malik clenched up before solos, so we rehearsed counting four silent beats before striking the bell tree. Logging each attempt on a progress chart proved to him that steady breathing and restraint can tame the worry he carries in.",
    "Reflection rounds had artists reread their zine entries, underline words tied to fear or hope, and then rewrite one stanza to show the shift. Sharing those drafts aloud left them buzzing about how revising with evidence from the text made their voices feel stronger.",
    "The new camper interview highlighted how staff coached a quick-to-shout drummer to tap the rim twice before speaking, giving peers room to weigh in. Charting discussion norms on the whiteboard doubled as a literacy task and she beamed about channeling her volume into support.",
    "Goal setting sessions have teens draft personal learning targets, then annotate lyrics to show where they pushed past stage fright. Keeping that evidence log convinces them their writing muscles grow with each revision.",
    "In improvisation lab the facilitator had everyone breathe twice, name the dominant feeling, and raise a hand before adding to the groove. They cited vocabulary from literacy night to explain their choices, and each success at pausing built their belief that they can stretch the next idea even further."
  ),
  `Resource Creator` = c(
    "Camp Director",
    "Evaluation Lead",
    "Alumni Coordinator",
    "Program Researcher",
    "School Liaison",
    "Behavior Specialist",
    "Camp Director",
    "Program Researcher",
    "Counselor Team Lead",
    "Behavior Specialist",
    "Evaluation Lead",
    "School Liaison",
    "Camp Director",
    "Behavior Specialist",
    "Program Researcher",
    "Counselor Team Lead",
    "Evaluation Lead",
    "Camp Director",
    "Program Researcher",
    "Counselor Team Lead"
  ),
  `Resource Date` = as.Date(c(
    "2023-06-01",
    "2023-07-12",
    "2023-12-20",
    "2023-07-20",
    "2023-08-02",
    "2023-08-09",
    "2023-08-16",
    "2023-08-23",
    "2023-08-30",
    "2023-09-06",
    "2023-09-13",
    "2023-09-20",
    "2023-09-27",
    "2023-10-04",
    "2023-10-11",
    "2023-10-18",
    "2023-10-25",
    "2023-11-01",
    "2023-11-08",
    "2023-11-15"
  ))
) |>
  mutate(
    range_stub = sub("Excerpt ", "", `Excerpt Range`),
    code_details = map2(
      code_assignments,
      range_stub,
      ~ tibble(
        code = .x,
        range = .y,
        weight = unname(code_weights[.x])
      )
    ),
    `Codes Applied Combined` = map_chr(
      code_details,
      ~ paste(intersect(code_order, .x$code), collapse = "; ")
    )
  )

for (code in code_order) {
  applied_col <- paste0("Code: ", code, " Applied")
  range_col <- paste0("Code: ", code, " Range")
  weight_col <- paste0("Code: ", code, " Weight")

  demo_excerpts[[applied_col]] <- map_chr(
    demo_excerpts$code_details,
    ~ if (code %in% .x$code) "True" else "False"
  )

  demo_excerpts[[range_col]] <- map_chr(
    demo_excerpts$code_details,
    ~ {
      rng <- .x$range[.x$code == code]
      if (length(rng) == 0) NA_character_ else rng
    }
  )

  demo_excerpts[[weight_col]] <- map_dbl(
    demo_excerpts$code_details,
    ~ {
      wt <- .x$weight[.x$code == code]
      if (length(wt) == 0) NA_real_ else wt
    }
  )
}

demo_excerpts <- demo_excerpts |>
  select(
    -range_stub,
    -code_details
  ) |>
  relocate(`Codes Applied Combined`, .after = `Resource Date`)

# Take a peeksie! ----------------------------------
demo_excerpts
#> # A tibble: 20 × 41
#>    `Media Title` `Excerpt Range` `Excerpt Creator` `Excerpt Date` `Excerpt Copy`
#>    <chr>         <chr>           <chr>             <date>         <chr>         
#>  1 Interview: O… Excerpt 12-56   Aliyah            2023-06-10     Keeping the d…
#>  2 Focus Group:… Excerpt 04-38   Rohan             2023-07-18     During the rh…
#>  3 Interview: A… Excerpt 77-112  Aliyah            2024-01-08     When I get ma…
#>  4 Classroom Ob… Excerpt 15-42   Lina              2023-07-24     In lyric lab …
#>  5 Journal Club… Excerpt 33-80   Mateo             2023-08-05     During the ca…
#>  6 Parent Inter… Excerpt 08-29   Priya             2023-08-12     On practice n…
#>  7 Coach Debrie… Excerpt 21-48   Lina              2023-08-19     During the st…
#>  8 Interview: T… Excerpt 44-78   Rohan             2023-08-26     The teen beat…
#>  9 Workshop: Be… Excerpt 63-97   Aliyah            2023-09-02     Aliyah's note…
#> 10 Focus Group:… Excerpt 10-35   Mateo             2023-09-09     In mentor cir…
#> 11 Observation:… Excerpt 52-85   Priya             2023-09-16     During homewo…
#> 12 Interview: C… Excerpt 18-41   Jordan            2023-09-23     At the closin…
#> 13 Survey Comme… Excerpt 71-103  Aliyah            2023-09-30     While recordi…
#> 14 Survey Comme… Excerpt 26-50   Rohan             2023-10-07     Rohan noted t…
#> 15 Interview: L… Excerpt 39-72   Jordan            2023-10-14     In leadership…
#> 16 Observation:… Excerpt 14-36   Priya             2023-10-21     During peer p…
#> 17 Focus Group:… Excerpt 58-90   Lina              2023-10-28     Reflection ro…
#> 18 Interview: N… Excerpt 23-47   Mateo             2023-11-04     The new campe…
#> 19 Observation:… Excerpt 66-101  Jordan            2023-11-11     Goal setting …
#> 20 Workshop: Im… Excerpt 30-63   Priya             2023-11-18     In improvisat…
#> # ℹ 36 more variables: `Resource Creator` <chr>, `Resource Date` <date>,
#> #   `Codes Applied Combined` <chr>, `Code: Motor Skills Applied` <chr>,
#> #   `Code: Motor Skills Range` <chr>, `Code: Motor Skills Weight` <dbl>,
#> #   `Code: Social Skills Applied` <chr>, `Code: Social Skills Range` <chr>,
#> #   `Code: Social Skills Weight` <dbl>, `Code: Self-expression Applied` <chr>,
#> #   `Code: Self-expression Range` <chr>, `Code: Self-expression Weight` <dbl>,
#> #   `Code: Emotional Regulation Applied` <chr>, …
```

## 1. Clean excerpts with `clean_data()`

The
[`clean_data()`](https://abiraahmi.github.io/DedooseR/reference/clean_data.md)
function standardizes column names, keeps the highest ranked coder per
transcript, drops range/weight columns, prefixes code variables with
`c_`, and returns both the cleaned data and a codebook. Supply the mock
data along with your preferred coder order. If you have single-coded
transcripts, you can simply list all your coders here. You can
optionally rename and relabel variables at the same time.

``` r
# Define your preferred coders ----------------------------------
preferred_coders <- c("Aliyah", "Rohan", "Lina", "Mateo", "Priya", "Jordan")

# Run the function ----------------------------------

cleaned <- clean_data(
  excerpts = demo_excerpts,
  preferred_coders = preferred_coders,
  rename_vars = list(resource_author = "resource_creator"),
  relabel_vars = list(
    media_title = "Interview or focus group title",
    resource_author = "Team member who uploaded to Dedoose"
  )
)

# Take a peeksie! ----------------------------------
cleaned$data
#> # A tibble: 20 × 19
#>    media_title              excerpt_creator excerpt_date excerpt resource_author
#>    <chr>                    <chr>           <date>       <chr>   <chr>          
#>  1 Interview: Opening Circ… Aliyah          2023-06-10   Keepin… Camp Director  
#>  2 Focus Group: Rhythm Rel… Rohan           2023-07-18   During… Evaluation Lead
#>  3 Interview: Alumni Showc… Aliyah          2024-01-08   When I… Alumni Coordin…
#>  4 Classroom Observation: … Lina            2023-07-24   In lyr… Program Resear…
#>  5 Journal Club: Campfire … Mateo           2023-08-05   During… School Liaison 
#>  6 Parent Interview: Pract… Priya           2023-08-12   On pra… Behavior Speci…
#>  7 Coach Debrief: Studio S… Lina            2023-08-19   During… Camp Director  
#>  8 Interview: Teen Beat Ma… Rohan           2023-08-26   The te… Program Resear…
#>  9 Workshop: Beat Mapping   Aliyah          2023-09-02   Aliyah… Counselor Team…
#> 10 Focus Group: Mentor Cir… Mateo           2023-09-09   In men… Behavior Speci…
#> 11 Observation: Homework J… Priya           2023-09-16   During… Evaluation Lead
#> 12 Interview: Closing Show… Jordan          2023-09-23   At the… School Liaison 
#> 13 Survey Comment: Quiet C… Aliyah          2023-09-30   While … Camp Director  
#> 14 Survey Comment: Confide… Rohan           2023-10-07   Rohan … Behavior Speci…
#> 15 Interview: Leadership H… Jordan          2023-10-14   In lea… Program Resear…
#> 16 Observation: Peer Pairi… Priya           2023-10-21   During… Counselor Team…
#> 17 Focus Group: Reflection… Lina            2023-10-28   Reflec… Evaluation Lead
#> 18 Interview: New Camper    Mateo           2023-11-04   The ne… Camp Director  
#> 19 Observation: Goal Setti… Jordan          2023-11-11   Goal s… Program Resear…
#> 20 Workshop: Improvisation… Priya           2023-11-18   In imp… Counselor Team…
#> # ℹ 14 more variables: resource_date <date>, codes_applied_combined <chr>,
#> #   c_motor_skills <lgl>, c_social_skills <lgl>, c_self_expression <lgl>,
#> #   c_emotional_regulation <lgl>, c_emotion_identification <lgl>,
#> #   c_inhibitory_control <lgl>, c_internalizing_behavior <lgl>,
#> #   c_externalizing_behavior <lgl>, c_academic_skills <lgl>,
#> #   c_self_esteem <lgl>, c_growth_mindset <lgl>, coder_rank <int>
```

Bonus: this function also produces a codebook, which allows you to get
an overview of your data and confirm your variable labels and types
before moving on.

``` r
cleaned$codebook
#>                                          variable
#> media_title                           media_title
#> excerpt_creator                   excerpt_creator
#> excerpt_date                         excerpt_date
#> excerpt                                   excerpt
#> resource_author                   resource_author
#> resource_date                       resource_date
#> codes_applied_combined     codes_applied_combined
#> c_motor_skills                     c_motor_skills
#> c_social_skills                   c_social_skills
#> c_self_expression               c_self_expression
#> c_emotional_regulation     c_emotional_regulation
#> c_emotion_identification c_emotion_identification
#> c_inhibitory_control         c_inhibitory_control
#> c_internalizing_behavior c_internalizing_behavior
#> c_externalizing_behavior c_externalizing_behavior
#> c_academic_skills               c_academic_skills
#> c_self_esteem                       c_self_esteem
#> c_growth_mindset                 c_growth_mindset
#> coder_rank                             coder_rank
#>                                                                        label
#> media_title                                   Interview or focus group title
#> excerpt_creator                                coder who created the excerpt
#> excerpt_date                                        date excerpt was created
#> excerpt                                                 full text of excerpt
#> resource_author                          Team member who uploaded to Dedoose
#> resource_date                     date media/transcript was added to Dedoose
#> codes_applied_combined                          all codes applied to excerpt
#> c_motor_skills                                                  motor skills
#> c_social_skills                                                social skills
#> c_self_expression                                            self-expression
#> c_emotional_regulation                                  emotional regulation
#> c_emotion_identification                              emotion identification
#> c_inhibitory_control                                      inhibitory control
#> c_internalizing_behavior                              internalizing behavior
#> c_externalizing_behavior                              externalizing behavior
#> c_academic_skills                                            academic skills
#> c_self_esteem                                                    self-esteem
#> c_growth_mindset                                              growth mindset
#> coder_rank               rank of coder, according to listed coder preference
#>                               type
#> media_title              character
#> excerpt_creator          character
#> excerpt_date                  Date
#> excerpt                  character
#> resource_author          character
#> resource_date                 Date
#> codes_applied_combined   character
#> c_motor_skills             logical
#> c_social_skills            logical
#> c_self_expression          logical
#> c_emotional_regulation     logical
#> c_emotion_identification   logical
#> c_inhibitory_control       logical
#> c_internalizing_behavior   logical
#> c_externalizing_behavior   logical
#> c_academic_skills          logical
#> c_self_esteem              logical
#> c_growth_mindset           logical
#> coder_rank                 integer
```

## 2. Recode related codes with `recode_themes()`

Qualitative analysis often requires a lot of code iteration. We begin
our journey with our research interests in mind, alongside an openness
to all else that may emerge. As we understand our data better, we begin
to see the patterns in ideas and often decide to tease apart or collapse
codes.

The
[`recode_themes()`](https://abiraahmi.github.io/DedooseR/reference/recode_themes.md)
function combines selected codes into a single logical column and
updates the codebook. Here, the two emotion-focused codes are collapsed
into one composite while the remaining themes stay as-is. In similar
fashion, you can tease apart codes.

``` r
recoded <- recode_themes(
  data = cleaned$data,
  recodes = list(
    c_emotional_growth = c(
      "c_emotional_regulation",
      "c_emotion_identification"
    )
  ),
  relabel_vars = list(
    c_emotional_growth = "emotional growth: identification and/or regulation"
  )
)

recoded$data_recode
#> # A tibble: 20 × 18
#>    media_title              excerpt_creator excerpt_date excerpt resource_author
#>    <chr>                    <chr>           <date>       <chr>   <chr>          
#>  1 Interview: Opening Circ… Aliyah          2023-06-10   Keepin… Camp Director  
#>  2 Focus Group: Rhythm Rel… Rohan           2023-07-18   During… Evaluation Lead
#>  3 Interview: Alumni Showc… Aliyah          2024-01-08   When I… Alumni Coordin…
#>  4 Classroom Observation: … Lina            2023-07-24   In lyr… Program Resear…
#>  5 Journal Club: Campfire … Mateo           2023-08-05   During… School Liaison 
#>  6 Parent Interview: Pract… Priya           2023-08-12   On pra… Behavior Speci…
#>  7 Coach Debrief: Studio S… Lina            2023-08-19   During… Camp Director  
#>  8 Interview: Teen Beat Ma… Rohan           2023-08-26   The te… Program Resear…
#>  9 Workshop: Beat Mapping   Aliyah          2023-09-02   Aliyah… Counselor Team…
#> 10 Focus Group: Mentor Cir… Mateo           2023-09-09   In men… Behavior Speci…
#> 11 Observation: Homework J… Priya           2023-09-16   During… Evaluation Lead
#> 12 Interview: Closing Show… Jordan          2023-09-23   At the… School Liaison 
#> 13 Survey Comment: Quiet C… Aliyah          2023-09-30   While … Camp Director  
#> 14 Survey Comment: Confide… Rohan           2023-10-07   Rohan … Behavior Speci…
#> 15 Interview: Leadership H… Jordan          2023-10-14   In lea… Program Resear…
#> 16 Observation: Peer Pairi… Priya           2023-10-21   During… Counselor Team…
#> 17 Focus Group: Reflection… Lina            2023-10-28   Reflec… Evaluation Lead
#> 18 Interview: New Camper    Mateo           2023-11-04   The ne… Camp Director  
#> 19 Observation: Goal Setti… Jordan          2023-11-11   Goal s… Program Resear…
#> 20 Workshop: Improvisation… Priya           2023-11-18   In imp… Counselor Team…
#> # ℹ 13 more variables: resource_date <date>, codes_applied_combined <chr>,
#> #   c_motor_skills <lgl>, c_social_skills <lgl>, c_self_expression <lgl>,
#> #   c_inhibitory_control <lgl>, c_internalizing_behavior <lgl>,
#> #   c_externalizing_behavior <lgl>, c_academic_skills <lgl>,
#> #   c_self_esteem <lgl>, c_growth_mindset <lgl>, coder_rank <int>,
#> #   c_emotional_growth <lgl>
```

``` r
recoded$codebook_recode
#>                                          variable
#> media_title                           media_title
#> excerpt_creator                   excerpt_creator
#> excerpt_date                         excerpt_date
#> excerpt                                   excerpt
#> resource_author                   resource_author
#> resource_date                       resource_date
#> codes_applied_combined     codes_applied_combined
#> c_motor_skills                     c_motor_skills
#> c_social_skills                   c_social_skills
#> c_self_expression               c_self_expression
#> c_inhibitory_control         c_inhibitory_control
#> c_internalizing_behavior c_internalizing_behavior
#> c_externalizing_behavior c_externalizing_behavior
#> c_academic_skills               c_academic_skills
#> c_self_esteem                       c_self_esteem
#> c_growth_mindset                 c_growth_mindset
#> coder_rank                             coder_rank
#> c_emotional_growth             c_emotional_growth
#>                                                                        label
#> media_title                                   Interview or focus group title
#> excerpt_creator                                coder who created the excerpt
#> excerpt_date                                        date excerpt was created
#> excerpt                                                 full text of excerpt
#> resource_author                          Team member who uploaded to Dedoose
#> resource_date                     date media/transcript was added to Dedoose
#> codes_applied_combined                          all codes applied to excerpt
#> c_motor_skills                                                  motor skills
#> c_social_skills                                                social skills
#> c_self_expression                                            self-expression
#> c_inhibitory_control                                      inhibitory control
#> c_internalizing_behavior                              internalizing behavior
#> c_externalizing_behavior                              externalizing behavior
#> c_academic_skills                                            academic skills
#> c_self_esteem                                                    self-esteem
#> c_growth_mindset                                              growth mindset
#> coder_rank               rank of coder, according to listed coder preference
#> c_emotional_growth        emotional growth: identification and/or regulation
#>                               type
#> media_title              character
#> excerpt_creator          character
#> excerpt_date                  Date
#> excerpt                  character
#> resource_author          character
#> resource_date                 Date
#> codes_applied_combined   character
#> c_motor_skills             logical
#> c_social_skills            logical
#> c_self_expression          logical
#> c_inhibitory_control       logical
#> c_internalizing_behavior   logical
#> c_externalizing_behavior   logical
#> c_academic_skills          logical
#> c_self_esteem              logical
#> c_growth_mindset           logical
#> coder_rank                 integer
#> c_emotional_growth         logical
```

These two objects—`recoded$data_recode` and
`recoded$codebook_recode`—are the ones we will carry into the follow-up
vignette on summarizing codes. Knit this article first so they are easy
to reuse.

## 3. Explore excerpts with `view_excerpts()`

Finally, review the excerpts behind each code.
[`view_excerpts()`](https://abiraahmi.github.io/DedooseR/reference/view_excerpts.md)
opens an interactive table powered by `DT`, letting you filter by code
and search within text.

``` r
view_excerpts(recoded$data_recode)
```

``` r
if (requireNamespace("DT", quietly = TRUE)) {
  view_excerpts(recoded$data_recode)
} else {
  message("Install the DT package to launch the interactive excerpt browser.")
}
```

TLDR: Start with
[`clean_data()`](https://abiraahmi.github.io/DedooseR/reference/clean_data.md)
to standardize variable names and labels, recode variables as you
iterate on your codebook, and lean on
[`view_excerpts()`](https://abiraahmi.github.io/DedooseR/reference/view_excerpts.md)
whenever you want to review the excerpts by code stories behind the
summaries.

To learn how to create code summaries and visualize them, continue to
the *Summarizing Codes* tutorial!
