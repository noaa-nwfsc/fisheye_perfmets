dat <- dbGetQuery(
    ifqpub,
    "select sum(number_response) num, edcsurvey_dbid,
case when fullcode like 'EX%' then 'PR_WG' else 'REV' end as flag
from edc_all_num_v where vessel_id = '933627' and survey_year = 2023 and (fullcode in ('EXCWWGPRCWC') or fullcode like 'RV%') and survey_type = 'MOTHERSHIP'
group by edcsurvey_dbid,
case when fullcode like 'EX%' then 'PR_WG' else 'REV' end"
)
