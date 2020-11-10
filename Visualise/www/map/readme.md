Map files are cached here for quicker loading. 

To download files you can install and use the RgoogleMaps package.
```
library(RgoogleMaps) 
GetMapTiles(lonR = c(-179, 179), latR = c(-80, 80), zoom = 6, type = "stamen-watercolor", tileDir = T)
```

Files are expected to be named as: z_x_y where z represents the zoom

These tiles are used in the leaflet function as follows:
`addTiles(urlTemplate = "map/tiles/{z}_{x}_{y}.jpg") %>% `

Alternatively, you can comment the above line out and use an online source, like so:
`addTiles() %>% addProviderTiles("Esri.WorldImagery") %>%`
