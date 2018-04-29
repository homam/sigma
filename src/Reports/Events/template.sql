with Views as (
  {$ select({ tableAlias: 'e' }) $}
  , sum(case when e.view then 1 else 0 end) :: int as views
  , sum(case when e.lead then 1 else 0 end) :: int as leads
  , sum(case when e.sale then 1 else 0 end) :: int as sales
  , sum(case when e.sale_pixel_direct or e.sale_pixel_delayed then 1 else 0 end) :: int as pixels
  , sum(case when(e.sale_pixel_direct or e.sale_pixel_delayed) and (e.scrub is not true) then 1 else 0 end) :: int as paid_sales
  , sum(case when e.firstbilling then 1 else 0 end) :: int as firstbillings
  , sum(case when e.optout then 1 else 0 end) :: int as day_optouts
  from public.events e 
  {$ where({ tableAlias: 'e' }) $}
  {$ groupBy() $}
  {$ orderBy() $}
)

, FirstBillings as (
  
  {$ select({ tableAlias: 's', casted: true }) $}
  , count(*) :: int as firstbillings from (
      {$ select({ tableAlias: 'b' }) $}
      , b.rockman_id 
    
    from events b
    
    inner join events s on 
          s.rockman_id = b.rockman_id
      and s.timestamp >= {$ dateFrom() $}
      and s.timestamp < {$ dateTo() $}
      and {$ filters({ tableAlias: 's' }) $}
      and s.sale
      
    where b.timestamp >= {$ dateFrom() $}
      and {$ filters({ tableAlias: 'b' }) $}
      and b.firstbilling

    {$ groupBy() $}, b.rockman_id
    
  ) as s 
  {$ groupBy() $}

)

, ReSubs as (
  with ReSubs1 as (
    {$ select({tableAlias: 'e'}) $}
    , e.msisdn as msisdn
    from public.events e 
    {$ where({tableAlias: 'e'}) $}
      and e.sale
    {$ groupBy() $}, msisdn
    {$ orderBy() $}, msisdn
  )
  
  {$ select({tableAlias: 'r', casted: true}) $}
  , count(*) :: int as uniquesales 
  from ReSubs1 r 
  {$ groupBy() $}
  {$ orderBy() $}

)

select v.*
, nvl(f.firstbillings, 0) as firstbillings
, nvl(r.uniquesales, 0) as uniquesales
from Views v
left join FirstBillings f on 
  {$ joinDimensions({tableAlias: 'v'}, {tableAlias: 'f'}) $}
left join ReSubs r on 
  {$ joinDimensions({tableAlias: 'v'}, {tableAlias: 'r'}) $}