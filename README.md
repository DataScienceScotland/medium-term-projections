
## Scottish Government Covid-19 Modelling

### Medium Term Projections

The Scottish Government produce projections and scenarios for the number
of infections and other measures in Scotland. These projections are done
in two parts. Firstly an epidemiological model is used to predict the
number of new infections. This projection is then split down by age
group, and a second (logistics) model is applied to predict the number
of people that will go to hospital, and potentially to ICU, based on
what we know about how different age groups react to the disease.

The main information feeding into our logistics model is:

  - our estimates of the number of people that will be infected with
    Covid;

  - data-based assumptions for the proportion of those that will
    experience different severities of the disease;

  - the amount of time it takes for infected people to reach different
    stages of the disease;

  - the length of time they are at each stage.

To model the people becoming more severely ill and therefore needing
hospital treatment, we use assumptions for the proportion of people
likely to require hospital treatment and the timeline of their disease.

The length of time we assume people stay in hospital is based on length
of stay distributions for viral pneumonia, with adjustments for Covid.
Again, these lengths of stay are age dependent, with younger people
likely to have shorter stays than older people.

The logistical model developed by Scottish Government to assess
implications for health care demand also produces a medium-term
projection of infections. This projection is submitted to SPI-M and
informs the consensus for daily incidence. The Scottish Government then
use Scottish Government modelling if it sits within the SPI-M consensus,
informed by several groups, to develop central, better and worse ends
for national incidence, Hospital Bed, ICU demand and deaths.

This model has been peer reviewed by the University of Edinburgh.

### Other Covid-19 Modelling

This model is one of a
[series](https://github.com/search?q=topic%3Ac19-modelling+org%3ADataScienceScotland+fork%3Atrue)
used by the Scottish Government during the pandemic to model the spread
and levels of Covid-19 in Scotland. Many of the results of these models
have been published in the [Modelling the
Epidemic](https://www.gov.scot/collections/coronavirus-covid-19-modelling-the-epidemic/)
series of reports.

If you have any questions regarding the contents of this repository,
please contact <sgcentralanalysisdivision@gov.scot>.

### Licence

This repository is available under the [Open Government Licence
v3.0](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
