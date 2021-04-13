library(plotly)

# Lorenz attractor --------------------------------------------------------

# sigma = 10; rho = 28; beta = 8/3
# x_init = 1; y_init = 1; z_init = 1
# t_max = 30; freq = 1/100

lorenz <- function(rho=28, sigma=10, beta=8/3, 
                   x_init=1, y_init=1, z_init=1, 
                   t_max=50, freq=1/100, cap=10000,
                   type="data", name=""){
  n_max = t_max/freq
  
  # limit data length if hard cap
  if(n_max > cap){
    n_max = cap
    freq = t_max/cap
  }
  
  x = numeric(n_max)
  y = numeric(n_max)
  z = numeric(n_max)
  
  x[1] = x_init
  y[1] = y_init
  z[1] = z_init
  
  for(tt in 2:n_max){
    t = tt-1
    dx = sigma * (y[t] - x[t])
    dy = x[t] * (rho - z[t]) - y[t]
    dz = x[t] * y[t] - beta * z[t]
    x[tt] = x[t] + freq * dx
    y[tt] = y[t] + freq * dy
    z[tt] = z[t] + freq * dz
  }
  
  if(type=="data") return(data.frame(x=x, y=y, z=z, t=seq(from=freq, to=t_max, by=freq), name=name))
  if(type=="3d") fig=plot_ly(x=x, y=y, z=z, type="scatter3d", mode="makers", size=I(1), name="xyz")
  if(type=="xy") fig=plot_ly(x=x, y=y, type="scatter", mode="makers", size=I(1), name="xy")
  if(type=="xz") fig=plot_ly(x=x, y=z, type="scatter", mode="makers", size=I(1), name="xz")
  if(type=="yz") fig=plot_ly(x=y, y=z, type="scatter", mode="makers", size=I(1), name="yz")
  return(fig)
}

noaxis <- list(visible = FALSE, showgrid = FALSE)
mstyle <- function(c, s=12) list(color = ~I(name), size = s, line = list(color = c,  width = 2))
mytit <- list(x = 1, y = 0.95, text = "Original Plots © Yan Pan 2021", font=list(size=9))

dropaxes <- list(list(
  active = -1,
  type = "buttons",
  buttons = list(
    list(method="relayout", label="Hide Axes", 
         args=list(list(
           scene=list(xaxis=noaxis, yaxis=noaxis, zaxis=noaxis)
         ))),
    list(method="relayout", label="Show Axes", 
         args=list(list(
           scene=list()
         )))
  )
))

dropmenu <- list(list(
  active = -1,
  type = "buttons",
  buttons = list(
    list(method="restyle", label="3D plot", 
         args=list(list(
           x=list(~x), y=list(~y), z=list(~z), color=~I(name), type=list("scatter3d")
         ))),
    list(method="restyle", label="Projection X-Y", 
         args=list(list(
           x=list(~x), y=list(~y), color=~I(name), type=list("scatter")
         ))),
    list(method="restyle", label="Projection X-Z", 
         args=list(list(
           x=list(~x), y=list(~z), color=~I(name), type=list("scatter")
         ))),
    list(method="restyle", label="Projection Y-Z", 
         args=list(list(
           x=list(~y), y=list(~z), color=~I(name), type=list("scatter")
         ))),
    list(method="update", label="Series X over t", 
         args=list(list(
           x=list(~t), y=list(~x), color=~I(name), type=list("scatter")
         ))),
    list(method="update", label="Series Y over t", 
         args=list(list(
           x=list(~t), y=list(~y), color=~I(name), type=list("scatter")
         ))),
    list(method="update", label="Series Z over t", 
         args=list(list(
           x=list(~t), y=list(~z), color=~I(name), type=list("scatter")
         )))
  )
))


testPlot <- function(){
  df1 <- lorenz(rho=28, name="#fbb4ae")
  df2 <- lorenz(rho=15, name="#b3cde3")
  
  rbind(df1, df2) %>%
    plot_ly() %>%
    add_trace(x=~x, y=~y, z=~z, color=~I(name), mode="lines", type="scatter3d") %>%
    layout(updatemenus=dropaxes)
#    layout(scene=list(xaxis=noaxis,yaxis=noaxis,zaxis=noaxis))
#    layout(updatemenus=dropmenu)
  
  subplot(
    plot_ly() %>%
      add_trace(data=df1, x=~x, y=~y, color=~I(name), mode="lines", type="scatter") %>%
      add_trace(data=df2, x=~x, y=~y, color=~I(name), mode="lines", type="scatter") %>%
      layout(showlegend=FALSE, title="Projections"),
    plot_ly() %>%
      add_trace(data=df1, x=~y, y=~z, color=~I(name), mode="lines", type="scatter") %>%
      add_trace(data=df2, x=~y, y=~z, color=~I(name), mode="lines", type="scatter") %>%
      layout(showlegend=FALSE),
    plot_ly() %>%
      add_trace(data=df1, x=~x, y=~z, color=~I(name), mode="lines", type="scatter") %>%
      add_trace(data=df2, x=~x, y=~z, color=~I(name), mode="lines", type="scatter") %>%
      layout(showlegend=FALSE),
    shareX = F, shareY = F, titleX = T, titleY = T, margin = 0.05)
  
  plot_ly() %>% 
    add_trace(data=df1, x=~x, y=~y, color=~I(name), mode="lines", type="scatter") %>%
    add_trace(data=df2, x=~x, y=~y, color=~I(name), mode="lines", type="scatter") %>%
    add_markers(data=df1 %>% filter(row_number() %% 20 == 1), x=~x, y=~y, frame=~t, size=I(10)) %>%
    add_markers(data=df2 %>% filter(row_number() %% 20 == 1), x=~x, y=~y, frame=~t, size=I(10))

  

  # animations --------------------------------------------------------------
  
  
  plot_ly() %>% 
    add_trace(data=df1, x=~x, y=~y, color=~I(name), mode="lines", type="scatter") %>%
    add_trace(data=df2, x=~x, y=~y, color=~I(name), mode="lines", type="scatter") %>%
    add_markers(data=df1 %>% filter(floor(10*t)==10*t), x=~x, y=~y, frame=~t, marker=mstyle("cyan")) %>%
    add_markers(data=df2 %>% filter(floor(10*t)==10*t), x=~x, y=~y, frame=~t, marker=mstyle("yellow"))

  plot_ly() %>% 
    add_trace(data=df1, x=~x, y=~y, z=~z, color=~I(name), mode="lines", type="scatter3d") %>%
    add_trace(data=df2, x=~x, y=~y, z=~z, color=~I(name), mode="lines", type="scatter3d") %>%
    add_markers(data=df1 %>% filter(floor(10*t)==10*t), x=~x, y=~y, z=~z, frame=~t, marker=mstyle("cyan")) %>%
    add_markers(data=df2 %>% filter(floor(10*t)==10*t), x=~x, y=~y, z=~z, frame=~t, marker=mstyle("yellow")) %>%
    layout(showlegend=FALSE, scene=list(xaxis=noaxis, yaxis=noaxis, zaxis=noaxis))
  
}


