about <- tabPanel("About",
  HTML(
    '<p style="text-align:justify">This R Shiny web application presents future climate outlooks for Northwest Territories (NT), Canada.
    Decadal maps show spatially explicit projections of temperature and precipitation based on SNAP\'s downscaled CMIP5 global climate models (GCMs) for three emissions scenarios.
    Climate trends are available as raw values or as changes from 1961-1990 baseline averages.
    When viewing raw values, available maps consist of individual GCMs or maps which aggregate or compare those models.</p>
    <p style="text-align:justify">NT communities are also available. When viewing a community summary, a time series plot will appear highlighting trends for the community.
    Even though the maps are at a decadal resolution, community data is annual.
    NT communities in the app are a small subset of those found in the Alaska and western Canada <a href="http://shiny.snap.uaf.edu/cc4liteFinal/">Community Charts R app</a>.</p>'),

  HTML('
    <div style="clear: left;"><img src="http://www.gravatar.com/avatar/52c27b8719a7543b4b343775183122ea.png" alt="" style="float: left; margin-right:5px" /></div>
    <p>Matthew Leonawicz<br/>
    Statistician | useR<br/>
    <a href="http://leonawicz.github.io" target="_blank">Github.io</a> |
    <a href="http://blog.snap.uaf.edu" target="_blank">Blog</a> |
    <a href="https://twitter.com/leonawicz" target="_blank">Twitter</a> |
    <a href="http://www.linkedin.com/in/leonawicz" target="_blank">Linkedin</a> <br/>
    <a href="http://www.snap.uaf.edu/", target="_blank">Scenarios Network for Alaska and Arctic Planning</a>
    </p>'),

  fluidRow(
    column(4,
      HTML('<strong>References</strong>
        <p></p>
        </ul>
        <li>Source code on <a href="https://github.com/ua-snap/shiny-apps/tree/master/ntwapp/" target="_blank">GitHub</a></li>
        </ul>')
      )
   ),
 value="about"
)
