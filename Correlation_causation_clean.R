if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("graph")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("RBGL")


install.packages('unifDAG')
library(unifDAG)

library(Rgraphviz)
library(graph)
library(igraph)

install.packages('rlang')

library(ggplot2)
install.packages("ggdag")
library(ggdag)
library(stringr)




#Translate between unifDAG and ggdag ########

dag_unif <- unifDAG(n=3)
plot(dag_unif)
dev.off()

empty_graph = function(v){
  paste("{",v,"}","->","{",v,"}")
}

transl_dag = function(dag_unif){
  
  n = length(dag_unif@nodes)
  
  if (length(dag_unif@edgeData@data) > 0) {
    
    for_dag = names(dag_unif@edgeData) %>% 
      str_replace(.,"[|]", " -> ") 
    
    for_comp = for_dag %>%
      str_split(.," -> ") %>%
      unlist() %>% unique()
    
    node_in = dag_unif@nodes %in% for_comp
    to_empty = dag_unif@nodes[!node_in]
    
    add = sapply(to_empty,empty_graph,USE.NAMES = FALSE)
    
    rel_dag = c(for_dag,add)
    
  } else {
    
    rel_dag = sapply(seq_len(as.numeric(n)),empty_graph)
    
  }
  
  return(rel_dag)
  
}

create_tidy_dag = function(num_nodes){
  
  dag_unif <- unifDAG(n=num_nodes)
  num_edges = length(dag_unif@edgeData@data)
  rel_dag_vec = transl_dag(dag_unif)
  b = do.call(dag, as.list(rel_dag_vec))
  tidy_dag <- tidy_dagitty(b)
  
  return(list(tidy_dag,num_edges))
}

#test create tidy dag
create_out <- create_tidy_dag(2)
ggdag(create_out[[1]])+theme_dag()

a1=create_out[[1]]
a2=create_out[[1]]
a3 = create_out[[1]]

a1$dag == a2$dag

dags = list(a1$dag,a2$dag)
dags2 = list(a1,a2)

a2$data %in% dags2

dags1 = vector(mode = "list", length = 10000)
dags_sceleton = vector(mode = "list", length = 10000)

for (i in 1:10000) {
  d  = create_tidy_dag(4)
  da = d[[1]]

  if (!da$dag %in% dags_sceleton) {
    dags1[[i]] = da
    dags_sceleton[[i]] = da$dag
  }
  
} #then find all pairs for n variables and calculate stats

sum(!dags_sceleton %in% list(NULL))


tidy_dag = create_out[[1]]
num_edges = create_out[[2]]
num_edges

install.packages("dagitty")
library(dagitty)

#plot tidy dag
ggdag(a2)+theme_dag()


is.descendant = function(t_dag,anc,des){
  
  de = node_descendants(t_dag,anc)
  
  d = de$data$name[de$data$descendant == "descendant"]
  
  fd = unique(d[!is.na(d)])
  
  return(des %in% fd)
  
}

is.child = function(t_dag,par,chi){
  
  ch = node_children(t_dag,par)
  
  c = ch$data$name[ch$data$children == "child"]
  
  fc = unique(c[!is.na(c)])
  
  return(chi %in% fc)
  
}

are.dconnected = function(t_dag,f,t){
  
  con = node_dconnected(t_dag,from = f,to = t)
  
  co = con$data$name[con$data$d_relationship == "d-connected"]
  
  fcon = unique(co[!is.na(co)])
  
  return(t %in% fcon)
}



find_colliders = function(tidy_dag){
  
  coll = node_collider(tidy_dag)
  
  c = coll$data$name[coll$data$colliders == "Collider"]
  
  return(unique(c[!is.na(c)]))
  
}

#tests on some common DAGs 
simple_chain = dag("{x} -> {m} m -> {y}")
simple_coll = dag("{x} -> {m} y -> {m}")
test_graph_1 = dag("{x b a} -> m m -> y" )
test_graph_2 = dag("{x y} -> m m -> a" )
test_graph_3 = dag("{1} -> {3 4} 3 -> 4 2 -> 2" )
test_graph_4 = dag("{1} -> {2 3 4} 3 -> {2 4} 4 -> 2" )
test_graph_5 = dag("{2} -> {1 3} 3 -> {1} 4 -> 5" )
test_graph_5 = dag("{3} -> {1 2} 4 -> {2 3} 5 -> 5" )
tidy_dag = tidy_dagitty(test_graph_3)

mbias = dag("{a} -> {m x} b -> {m y}")
tidy_dag = tidy_dagitty(mbias)

m_p_bias = dag("{a} -> {m x} b -> {m y} x -> y y -> y")
b_bias = dag("{a} -> {m x} b -> {m y} m -> {x y}")
b_p_bias = dag("{a} -> {m x} b -> {m y} m -> {x y} x -> y")
tidy_dag = tidy_dagitty(b_p_bias)

bias_t_1 = dag("{a} -> {x y b} c -> {b} b -> b x -> x")
bias_t_2 = dag("{a} -> {x y b} x -> y c -> {b} b -> b")
bias_t_3 = dag("{x} -> {a c} a -> {y} y -> y {c d} -> f d-> d")
bias_t_4 = dag("{x y} -> {a} x -> {c} y -> y {c d} -> f d-> d")
tidy_dag = tidy_dagitty(bias_t_1)

ggdag(tidy_dag)+theme_dag()

find_colliders(tidy_dag)
num_paths_opened_by_cond(tidy_dag,'1','2','3')

f = '1'
t = '2'
z = '3'

num_paths_opened_by_cond = function(tidy_dag,f,t,z){
  
  decom_1 = (str_split(tidy_dag$dag,"\n") %>% unlist())
  decom_2 = grepl("->", decom_1, fixed = TRUE)
  ed = decom_1[decom_2]
  edd = str_split(ed, " -> ") %>% unlist()
  
  g <- graph(edd)
  decom_g = decompose.graph(g)
  
  
  if (length(decom_g) > 1) {
    
    vert_list = sapply(decom_g, V) #V: igraph function giving a sequence vertices
    
    names_list = sapply(vert_list, names)
    
    m_ele_s = sapply(names_list, is.element, el = c(f,t)) %>% rowSums()
    
    if (2 %in% m_ele_s) {
      print('2 is in')
      path = dag_paths(tidy_dag,from = f, to = t,directed = FALSE)
      path_new = dag_paths(tidy_dag,from = f, to = t,directed = FALSE,adjust_for = z)
      
      if (nrow(path$data) == 0){
        num_p = 0
        
      } else {
        num_p = length(unique(path$data$set))
      }
      
      if (nrow(path_new$data) == 0){
        num_p_new = 0
        
      } else {
        num_p_new = length(unique(path_new$data$set))
      }
      
    } else {
      
      num_p = 0
      num_p_new = 0
      
    }
    
  } else {
    path = dag_paths(tidy_dag,from = f, to = t,directed = FALSE)
    path_new = dag_paths(tidy_dag,from = f, to = t,directed = FALSE,adjust_for = z)
    
    if (nrow(path$data) == 0){
      num_p = 0
      
    } else {
      num_p = length(unique(path$data$set))
    }
    
    if (nrow(path_new$data) == 0){
      num_p_new = 0
      
    } else {
      num_p_new = length(unique(path_new$data$set))
    }
  }
  
  return(num_p_new-num_p)

  
}




##### Number of adjustment sets #######


sim_prelim = function(l,num_nod){
  
  nod = replicate(l,sample(1:num_nod,2,replace = FALSE))
  one = as.character(nod[1,])
  two = as.character(nod[2,])
  numCores <- detectCores()
  dags = mclapply(rep(num_nod, l), create_tidy_dag,mc.cores = numCores) #try sampling w/o replacement
  
  return(list(dags, one, two))
  
}

##### Main simulation function. Input: tidy_dag, and two randomly chosen nodes (all from sim_prelim function) #####
simulate = function(tidy_dagg,one,two){
  
  time = c()
  
  tidy_dag = tidy_dagg[[1]] #get DAG
  
  edges = tidy_dagg[[2]] #number of edges in DAG
  
  coll = find_colliders(tidy_dag) #includes downstream colliders
  num_colliders = length(coll) #number of all colliders in DAG
  
  if (num_colliders == 0) {
    num_coll_open_path = -1
    num_paths_opened = 0
    time = 0
    
  } else {
    di = !(coll %in% c(one,two)) #exclude one and two from the collider list
    diff = coll[di]
    #print(paste("diff =",diff))
    
    if (length(diff) > 0) {
      #print(tidy_dag)
      #print(paste("one =",one,"two =",two))
      tt1 <- Sys.time()
      num_path_each_coll = sapply(diff, num_paths_opened_by_cond,tidy_dag = tidy_dag, f = one, t = two,USE.NAMES = FALSE)
      tt2 <- Sys.time()
      dt = difftime(tt2, tt1) #this is accounting for 2/3 of all time
      #print(paste0('Time to run num_paths_opened_by_cond was: ',round(dt,2)," ",attr(dt,'units')))
      time = c(time,dt)
      
      #print(paste("num_paths_each_coll =",num_path_each_coll))
      num_coll_open_path = length(which(num_path_each_coll > 0)) #counts num of colliders that open more paths than they close
      num_paths_opened = sum(num_path_each_coll) #total num of paths opened (or closed) by all colliders in DAG (could be negative)
      
    } else {
      num_coll_open_path = 0
      num_paths_opened = 0
      time = 0
    }
    
  }
  
  dconn = are.dconnected(tidy_dag,one,two) #are nodes d-connected (on an empty set)?
  causal_path = is.descendant(tidy_dag,anc = one,des = two) #is there a causal path between one and two?
  child = is.child(tidy_dag,par = two,chi = one) #is one a child of two (i.e. two -> one)?
  
  #find adjustment sets: this blows up with the number of nodes
  dg = dag_adjustment_sets(tidy_dag,exposure = one,outcome = two,type = c("all"),
                           effect = c("total"))
  sets = unique(dg$data$set)
  
  #check if no way to find adjustment set; should coincide with one being a child of two (i.e. child = TRUE) and num_adjust = 0
  cannot_block = (sets[1] == "{(No Way to Block Backdoor Paths)}")
  
  #check if one of adjustment sets is an empty set
  adj_empty = ("{(Backdoor Paths Unconditionally Closed)}" %in% sets)
  
  
  if ( length(sets) == 1 & sets[1] == "{(No Way to Block Backdoor Paths)}") {
    num_adjust = 0
    
  } else {
    num_adjust = length(sets)
  }

  return(data.frame(num_adjust = num_adjust, child = child, causal_path = causal_path,
                    dconn=dconn, edges=edges, num_colliders = num_colliders,
                    cannot_block = cannot_block, adj_empty = adj_empty, num_paths_opened = num_paths_opened,
                    num_coll_open_path = num_coll_open_path,time=time))
  
}






library(tidyverse)

#function to run the whole simulation
run_sim = function(){
  st1 <- Sys.time()
  li = sim_prelim(l,num_nod)
  et1 <- Sys.time()
  
  st2 <- Sys.time()
  sim =  pmap_dfr(li, simulate)
  et2 <- Sys.time()
  
  td1 = difftime(et1, st1)
  td2 = difftime(et2, st2)
  
  print(paste0('Time to sample ',l,' DAGs of ',num_nod,' nodes was: ',round(td1,2)," ",attr(td1,'units')))
  print(paste0('Time to calculate the metrics was: ',round(td2,2)," ",attr(td2,'units')))
  return(sim)
}

#function to run the whole simulation and calculate the elapsed time
run_sim_time = function(){
  start_time <- Sys.time()
  sim_res = run_sim()
  end_time <- Sys.time()
  
  time_diff = difftime(end_time, start_time)
  print(paste0('Time to run ',l,' samples of ',num_nod,' nodes was: ',round(time_diff,2)," ",attr(time_diff,'units')))
  return(sim_res)
}

####### Run simulation l times with DAGs of num_nodes nodes #######

min_node = 3 #minimum number of nodes
max_node = 3 #maximum number of nodes
data_DAGs <- vector(mode = "list", length = (max_node-min_node+1)) #create a list to store data
num_nod_vec = rep(NA,(max_node-min_node+1)) #create a vector to track the number of nodes

{
  start_t  <- Sys.time()
  print(start_t)
  for (i in seq_len(max_node-min_node + 1)){
    l = 5000 #number of samples
    num_nod = i + min_node - 1
    num_nod_vec[i] = num_nod
    print(paste0('Running simulation with ',num_nod,' nodes ...'))
    data_DAGs[[i]] = run_sim_time()
    
  }
  end_t <- Sys.time()
  print(end_t)
  print('All done!')
  time_diff_all = difftime(end_t, start_t)
  print(paste0('Time to run the whole simulation with ',l,' samples of ',min_node,' to ',max_node,' nodes was: ',round(time_diff_all,2)," ",attr(time_diff_all,'units')))
}

save(data_DAGs, file="data_DAG_11-15_3k.RData") #save data

install.packages("sendmailR")
library(sendmailR)

#get an email when a simulation is finished
#frr <- sprintf("<markot375@gmail.com>","The Sender") # the sender’s name is an optional value
#tto <- sprintf("<markot375@gmail.com>")
#subject_line <- "Simulations done"
#body_text <- "Simulations between 11 and 15 nodes are done!"

#sendmail(frr,tto,subject_line,body_text,control=list(smtpServer= "ASPMX.L.GOOGLE.COM"))


######### Load data ##########
load("data_DAG_11-15_3k.RData")

data_DAG_3_6_10k = data_DAGs
data_DAG_3_6_30k = data_DAGs
data_DAG_3_6_40k = list(rbind(data_DAG_3_6_30k[[1]],data_DAG_3_6_10k[[1]]),rbind(data_DAG_3_6_30k[[2]],data_DAG_3_6_10k[[2]]),
                        rbind(data_DAG_3_6_30k[[3]],data_DAG_3_6_10k[[3]]),rbind(data_DAG_3_6_30k[[4]],data_DAG_3_6_10k[[4]]))



data_DAG_7_10k = data_DAGs[1]
data_DAG_8_10_10k = data_DAGs
data_DAG_11_13_3k = data_DAGs[1:3]

all_data_DAGs = c(data_DAG_3_6_40k,data_DAG_7_10k,data_DAG_8_10_10k,data_DAG_11_13_3k)

nodes = 3:13

all_data_DAGs_nodes = mapply(cbind,all_data_DAGs,'nodes'=nodes,SIMPLIFY = F)

library(data.table)
df_all_data_DAGs_nodes = rbindlist(all_data_DAGs_nodes,use.names=T,idcol=T)
df_all_data_DAGs_nodes$density = (df_all_data_DAGs_nodes$edges*2)/df_all_data_DAGs_nodes$nodes
df_all_data_DAGs_nodes$density_e = (df_all_data_DAGs_nodes$edges)/((df_all_data_DAGs_nodes$nodes*(df_all_data_DAGs_nodes$nodes-1))/2)

setwd(dir = "/Users/marko/Dropbox/Postdoc UK IC/Correlation_causation")
getwd()
rm(list = ls())

#if want to run a simulation with DAGs on only num_nod nodes
#l = 1000
#num_nod = 7
#sim_res = run_sim_time()



library(psych)

basic_stat = function(sim_res){
  string_names = c("child","causal path","dconn","adj set empty")
  data = data.frame(string_names = string_names,
                    value = c(sum(sim_res$child)/l,sum(sim_res$causal_path)/l,
                              sum(sim_res$dconn)/l,sum(sim_res$adj_empty)/l)
  )
  return(data)
}

sim_r_e_c = function(sim_res){
  sim_res_empty_coll = sim_res
  sim_res_empty_coll$num_coll_open_path_0 = sim_res_empty_coll$num_coll_open_path
  sim_res_empty_coll$num_coll_open_path_0[sim_res_empty_coll$num_coll_open_path_0 == -1] = 0
  sim_res_empty_coll$empty_collider_0 = sim_res_empty_coll$num_coll_open_path_0 * sim_res_empty_coll$adj_empty
  sim_res_empty_coll$empty_collider = sim_res_empty_coll$num_coll_open_path * sim_res_empty_coll$adj_empty
  sim_res_empty_coll$empty_collider[sim_res_empty_coll$num_coll_open_path == -1] = -1
  return(sim_res_empty_coll)
}

#function to create a data.frame for stratifying adjustment sets by edges 
df_plot = function(sim_res){
  
  df_counting_plot = sim_res %>% 
    dplyr::group_by(as.factor(edges)) %>%
    dplyr::summarise(num_adjust = sum(num_adjust))
  names(df_counting_plot) = c("edges",'num_adjust_sets')
  df_counting_plot$prop_adjust_sets = df_counting_plot$num_adjust_sets/sum(df_counting_plot$num_adjust_sets)
  
  return(df_counting_plot)
}

#function to create a data.frame for stratifying colliders by edges 
df_plot_coll = function(sim_res){
  
  df_counting_plot = sim_res %>% 
    dplyr::group_by(as.factor(edges)) %>%
    dplyr::summarise(num_colliders = sum(num_colliders))
  names(df_counting_plot) = c("edges",'num_colliders')
  df_counting_plot$prop_colliders = df_counting_plot$num_colliders/sum(df_counting_plot$num_colliders)
  
  return(df_counting_plot)
}

#function to create a data.frame for stratifying open path increasing colliders by edges 
df_plot_coll_open = function(sim_res){
  
  df_counting = sim_res[sim_res$num_coll_open_path >= 0,]
  
  df_counting_plot = df_counting %>% 
    dplyr::group_by(as.factor(edges)) %>%
    dplyr::summarise(num_coll_open_path = sum(num_coll_open_path))
  names(df_counting_plot) = c("edges",'num_coll_open_path')
  df_counting_plot$prop_coll_open_path = df_counting_plot$num_coll_open_path/sum(df_counting_plot$num_coll_open_path)
  
  return(df_counting_plot)
}

#function to create a data.frame for stratifying open path increasing colliders by edges when one of the adjustment sets is an empty set
df_plot_coll_open_empty = function(sim_res){
  
  df_counting_plot = sim_res %>% 
    dplyr::group_by(as.factor(edges)) %>%
    dplyr::summarise(empty_collider_0 = sum(empty_collider_0))
  names(df_counting_plot) = c("edges",'empty_collider_0')
  df_counting_plot$prop_coll_open_path_empty = df_counting_plot$empty_collider_0/sum(df_counting_plot$empty_collider_0)
  
  return(df_counting_plot)
}


install.packages("gridExtra")
library(gridExtra)

plot_everything = function(sim_res,l,num_nod){
  
  sim_res_empty_coll = sim_r_e_c(sim_res)
  #print(sim_res_empty_coll)
  
  cont_desc = describe(sim_res_empty_coll[,c("num_adjust","edges","num_colliders","num_coll_open_path_0","empty_collider_0")])
  
  #print(cont_desc)
  
  cont_desc_df = data.frame(vars = c("#adjust sets","#edges","#colliders","#colliders \n open path","#colliders \n open path \n {} in adj. set"), mean_c = cont_desc$mean, se = cont_desc$se)
  
  #print(cont_desc_df)
  
  #basic stats on DAGs: cont vars
  b_stat_cont_p = ggplot(cont_desc_df, aes(x=vars)) + 
    geom_bar(aes(y=mean_c), stat="identity", alpha=1) +
    geom_errorbar(aes(ymin = mean_c - se, ymax = mean_c + se), width=0.2)+
    theme_bw()+theme(axis.text.x=element_text(angle=0, size=9, vjust=0.5),axis.text.y=element_text(size = 9),axis.title.x = element_text(size = 10.5),axis.title.y = element_text(size = 10),legend.text = element_text(size = 11))+
    labs(x="", y="Mean")
  
  b_stat = basic_stat(sim_res)
  
  #basic stats on DAGs: disc vars
  b_stat_disc_p = ggplot(b_stat, aes(x=string_names, y=value)) + 
    geom_bar(stat = "identity")+
    theme_bw()+theme(axis.text.x=element_text(angle=0, size=9, vjust=0.5),axis.text.y=element_text(size = 9),axis.title.x = element_text(size = 10.5),axis.title.y = element_text(size = 10),legend.text = element_text(size = 11))+
    labs(x="", y="Prop. of sampled DAGs")
  
  
  #plot proportions of DAGs with num_adjust adjustment sets
  adj_set_p = ggplot(sim_res, aes(x=as.factor(num_adjust))) + 
    geom_bar(aes(y=..prop..,group = 1))+
    theme_bw()+theme(axis.text.x=element_text(angle=90, size=9, vjust=0.5),axis.text.y=element_text(size = 9),axis.title.x = element_text(size = 10.5),axis.title.y = element_text(size = 10),legend.text = element_text(size = 11))+
    labs(x="Number of valid adjustment sets", y="Prop. of sampled DAGs")
  
  #plot proportions of DAGs with num_colliders colliders
  num_coll_p = ggplot(sim_res, aes(x=as.factor(num_colliders))) + 
    geom_bar(aes(y=..prop..,group = 1))+
    theme_bw()+theme(axis.text.x=element_text(angle=90, size=9, vjust=0.5),axis.text.y=element_text(size = 9),axis.title.x = element_text(size = 10.5),axis.title.y = element_text(size = 10),legend.text = element_text(size = 11))+
    labs(x="Number of colliders in DAGs", y="Prop. of sampled DAGs")
  
  #plot proportions of DAGs with num_colliders that opened paths which increase the number of open paths compared to before conditioning on that collider
  num_coll_incr_p = ggplot(sim_res, aes(x=as.factor(num_coll_open_path))) + 
    geom_bar(aes(y=..prop..,group = 1))+
    theme_bw()+theme(axis.text.x=element_text(angle=90, size=9, vjust=0.5),axis.text.y=element_text(size = 9),axis.title.x = element_text(size = 10.5),axis.title.y = element_text(size = 10),legend.text = element_text(size = 11))+
    labs(x="Number of colliders that increase # of open paths", y="Prop. of sampled DAGs")
  
  
  #plot proportions of DAGs with num_colliders that opened paths which increase the number of open paths compared to before conditioning on that collider, when one of the adjustment sets is an empty set
  num_coll_incr_empty_p = ggplot(sim_res_empty_coll, aes(x=as.factor(empty_collider))) + 
    geom_bar(aes(y=..prop..,group = 1))+
    theme_bw()+theme(axis.text.x=element_text(angle=90, size=9, vjust=0.5),axis.text.y=element_text(size = 9),axis.title.x = element_text(size = 10.5),axis.title.y = element_text(size = 10),legend.text = element_text(size = 11))+
    labs(x="Number of colliders that increase # of open paths \n when {} in adjus. sets", y="Prop. of sampled DAGs")
  
  
  #plot proportions of DAGs with edges number of edges
  edges_p = ggplot(sim_res, aes(x=as.factor(edges))) + 
    geom_bar(aes(y=..prop..,group = 1))+
    theme_bw()+theme(axis.text.x=element_text(angle=90, size=9, vjust=0.5),axis.text.y=element_text(size = 9),axis.title.x = element_text(size = 10.5),axis.title.y = element_text(size = 10),legend.text = element_text(size = 11))+
    labs(x="Number of edges", y="Prop. of sampled DAGs")
  
  
  
  #function to create a data.frame for stratifying adjustment sets by edges 
  df_counting_plot = df_plot(sim_res)
  
  adj_set_by_edges_p = ggplot(df_counting_plot,aes(x=edges,y=prop_adjust_sets))+
    geom_bar(stat = "identity") +
    theme_bw()+theme(axis.text.x=element_text(angle=90, size=9, vjust=0.5),axis.text.y=element_text(size = 9),axis.title.x = element_text(size = 10.5),axis.title.y = element_text(size = 10),legend.text = element_text(size = 11))+
    labs(x = "Number of edges", y = "Adjustment sets \n (as a prop. of the tot. num.)")
  
  
  #function to create a data.frame for stratifying colliders by edges 
  df_counting_coll_edges = df_plot_coll(sim_res)
  
  coll_by_edges_p = ggplot(df_counting_coll_edges,aes(x=edges,y=prop_colliders))+
    geom_bar(stat = "identity") +
    theme_bw()+theme(axis.text.x=element_text(angle=90, size=9, vjust=0.5),axis.text.y=element_text(size = 9),axis.title.x = element_text(size = 10.5),axis.title.y = element_text(size = 10),legend.text = element_text(size = 11))+
    labs(x = "Number of edges", y = "Colliders \n (as a prop. of the tot. num.)")
  
  
  #function to create a data.frame for stratifying open path increasing colliders by edges 
  df_counting_coll_edges_open = df_plot_coll_open(sim_res)
  
  coll_increase_by_edges_p = ggplot(df_counting_coll_edges_open,aes(x=edges,y=prop_coll_open_path))+
    geom_bar(stat = "identity") +
    theme_bw()+theme(axis.text.x=element_text(angle=90, size=9, vjust=0.5),axis.text.y=element_text(size = 9),axis.title.x = element_text(size = 10.5),axis.title.y = element_text(size = 10),legend.text = element_text(size = 11))+
    labs(x = "Number of edges", y = "Colliders increasing # of open paths \n (as a prop. of the tot. num.)")
  
  
  #function to create a data.frame for stratifying open path increasing colliders by edges when one of the adjustment sets is an empty set
  df_counting_coll_edges_open_empty = df_plot_coll_open_empty(sim_res_empty_coll)
  
  coll_increase_empty_by_edges_p = ggplot(df_counting_coll_edges_open_empty,aes(x=edges,y=prop_coll_open_path_empty))+
    geom_bar(stat = "identity") +
    theme_bw()+theme(axis.text.x=element_text(angle=90, size=9, vjust=0.5),axis.text.y=element_text(size = 9),axis.title.x = element_text(size = 10.5),axis.title.y = element_text(size = 10),legend.text = element_text(size = 11))+
    labs(x = "Number of edges", y = "Colliders increasing # of open paths \n when {} in adjust. sets \n (as a prop. of the tot. num.)")
  
  
  return(list(b_stat_cont_p = b_stat_cont_p, b_stat_disc_p= b_stat_disc_p,
              adj_set_p = adj_set_p, num_coll_p = num_coll_p,num_coll_incr_p = num_coll_incr_p,
              num_coll_incr_empty_p = num_coll_incr_empty_p,
              edges_p = edges_p, adj_set_by_edges_p = adj_set_by_edges_p,
              coll_by_edges_p = coll_by_edges_p, coll_increase_by_edges_p = coll_increase_by_edges_p,
              coll_increase_empty_by_edges_p = coll_increase_empty_by_edges_p))
  
}



#grid.arrange()

#Change index to select different data frame
sim_res = data_DAG_11_13_3k[[3]]
num_nod_vec = 11:13
l=3000 #number of samples
num_node_p = num_nod_vec[3]

list_plot = plot_everything(sim_res,l = l,num_nod = num_node_p)

{
  p_1 = arrangeGrob(list_plot[["b_stat_cont_p"]], list_plot[["b_stat_disc_p"]], list_plot[["adj_set_p"]], list_plot[["num_coll_p"]], list_plot[["num_coll_incr_p"]], list_plot[["num_coll_incr_empty_p"]], nrow = 3,ncol = 2,top=paste(l,"sampled DAGs with",num_node_p,"nodes"))
  
  
  
  ggsave( paste(num_node_p,"_nodes_",l/1000,"k_1.pdf",sep = ""),p_1, width = 20, height = 25, units = "cm",
          path = "/Users/marko/Dropbox/Apps/Overleaf/Causation_corrleation")
  
  lay <- rbind(c(1,1,2,2),
               c(3,3,4,4),
               c(NA,5,5,NA))
  
  #grid.arrange()
  
  p_2 = arrangeGrob(list_plot[["edges_p"]], list_plot[["adj_set_by_edges_p"]], list_plot[["coll_by_edges_p"]], list_plot[["coll_increase_by_edges_p"]], list_plot[["coll_increase_empty_by_edges_p"]], layout_matrix = lay,top=paste(l,"sampled DAGs with",num_node_p,"nodes"))
  
  ggsave( paste(num_node_p,"_nodes_",l/1000,"k_2.pdf",sep = ""),p_2, width = 20, height = 25, units = "cm",
          path = "/Users/marko/Dropbox/Apps/Overleaf/Causation_corrleation")
}


###### Descriptive stats ######

prop_names = c("child","causal_path","dconn","adj_set_empty")

list_l = length(all_data_DAGs)

desc_data = data.frame(num_nodes = rep(NA,list_l), child = rep(NA,list_l), causal_path = rep(NA,list_l), dconn = rep(NA,list_l), adj_set_empty = rep(NA,list_l), adjust_sets = rep(NA,list_l), edges = rep(NA,list_l), colliders = rep(NA,list_l), colliders_o_p = rep(NA,list_l), colliders_o_p_E = rep(NA,list_l),adjust_sets_SE = rep(NA,list_l), edges_SE = rep(NA,list_l), colliders_SE = rep(NA,list_l), colliders_o_p_SE = rep(NA,list_l), colliders_o_p_E_SE = rep(NA,list_l)) 

cont_names = names(desc_data)[6:15]

for (i in seq_len(list_l)){
  
  df_sim = all_data_DAGs[[i]]
  
  l = nrow(all_data_DAGs[[i]])
  
  desc_data$num_nodes[i] = i+2
  
  props = c(sum(df_sim$child)/l,sum(df_sim$causal_path)/l,
            sum(df_sim$dconn)/l,sum(df_sim$adj_empty)/l)
  
  desc_data[i,prop_names] = props
  
  df_empty_coll = sim_r_e_c(df_sim)
  
  cont_desc = describe(df_empty_coll[,c("num_adjust","edges","num_colliders","num_coll_open_path_0","empty_collider_0")])
  
  
  cont_v = c(cont_desc$mean,cont_desc$se)
  
  desc_data[i,cont_names] = cont_v
  #print(cont_desc)
  
  #cont_desc_df = data.frame(vars = cont_names, mean_c = cont_desc$mean, se = cont_desc$se)
  
  
}


names_x = c("Adj sets","Edges","Colliders","Coll open path")

library(reshape2)
library(dplyr)
######## Group by density and nodes ############

#correlation between nodes and density
df_corr_nodes_density = df_all_data_DAGs_nodes %>% 
  dplyr::group_by(as.factor(nodes)) %>%
  dplyr::summarise(density_m = mean(density),
                   density_sd = sd(density))

df_corr_nodes_density = rename(df_corr_nodes_density,nodes = `as.factor(nodes)`)

corr_nodes_density = ggplot(df_all_data_DAGs_nodes,aes(as.factor(nodes),density))+#group = 1
  #geom_errorbar(aes(ymin=density_m-density_sd, ymax=density_m+density_sd), width=.3) +
  #geom_point(size=4,alpha=.01)+
  geom_bin2d(bins = 30, color = "gray")+
  stat_bin2d(geom = "text", aes(label = ..count..),bins = 30)+
  scale_fill_distiller(palette = "YlOrRd", direction = 1,limits=c(0,3000)) +
  guides(fill = guide_colourbar(title = "Number of DAGs"))+
  #geom_violin(trim = T)+
  #geom_line(size=1)+
  theme_bw()+theme(axis.text.x=element_text(angle=0, size=11, vjust=0.5),axis.text.y=element_text(size = 11),axis.title.x = element_text(size = 13),axis.title.y = element_text(size = 13),legend.text = element_text(size = 12),legend.position="right")+
  scale_y_continuous(breaks=seq(0,9,.5))+
  #scale_x_continuous(breaks=seq(3,13,1))+
  labs(x="Number of nodes in DAGs", y="Density (average neighbourhood size of a node)")
  #geom_smooth()
corr_nodes_density

nodes_n = as.numeric(levels(df_corr_nodes_density$nodes))[df_corr_nodes_density$nodes]
cor.test(df_all_data_DAGs_nodes$nodes,df_all_data_DAGs_nodes$density_e)

#replicate with simpler plotting and grouping all data in one data.frame: by nodes
df_props_nodes = df_all_data_DAGs_nodes %>% 
  dplyr::group_by(as.factor(nodes)) %>%
  dplyr::summarise(child_p = sum(child)/n(),
                   causal_path_p = sum(causal_path)/n(),
                   dconn_p = sum(dconn)/n(),
                   adj_empty_p = sum(adj_empty)/n())

df_props_nodes = rename(df_props_nodes,nodes = `as.factor(nodes)`)
df_props_nodes_l = melt(df_props_nodes, id.vars=c("nodes"))

sem <- function(x) sd(x)/sqrt(length(x))

df_all_data_DAGs_nodes$num_coll_open_path_0 = df_all_data_DAGs_nodes$num_coll_open_path
df_all_data_DAGs_nodes$num_coll_open_path_0[df_all_data_DAGs_nodes$num_coll_open_path == -1] = 0

df_means_nodes = df_all_data_DAGs_nodes %>% 
  dplyr::group_by(as.factor(nodes)) %>%
  dplyr::summarise(variable = c('num_adjust','edges','num_colliders','num_coll_open_path_0'),
                   means = c(mean(num_adjust),mean(edges),mean(num_colliders),mean(num_coll_open_path_0)),
                   se = c(sem(num_adjust),sem(edges),sem(num_colliders),sem(num_coll_open_path_0)))
                   #num_adjust_sd = sd(num_adjust),
                   #edges_m = mean(edges),
                   #edges_sd = sd(edges),
                   #num_colliders_m = mean(num_colliders),
                   #num_colliders_sd = sd(num_colliders),
                   #num_coll_open_path_m = mean(num_coll_open_path),
                   #num_coll_open_path_sd = sd(num_coll_open_path))

df_means_nodes = rename(df_means_nodes,nodes = `as.factor(nodes)`)
df_means_nodes$variable = factor(df_means_nodes$variable,levels=c('num_adjust','edges','num_colliders','num_coll_open_path_0'))
#df_means_nodes_l = melt(df_means_nodes, id.vars=c("nodes"))

df_all_data_DAGs_nodes$coll_nodes = df_all_data_DAGs_nodes$num_colliders/df_all_data_DAGs_nodes$nodes
df_all_data_DAGs_nodes$coll_open_nodes = df_all_data_DAGs_nodes$num_coll_open_path_0/df_all_data_DAGs_nodes$nodes

df_means_nodes_props = df_all_data_DAGs_nodes %>% 
  dplyr::group_by(as.factor(nodes)) %>%
  dplyr::summarise(variable = c('prop_colliders','prop_coll_open_path_0'),
                   means = c(mean(coll_nodes),mean(coll_open_nodes)),#num_colliders/nodes num_coll_open_path_0/nodes
                   se = c(sem(coll_nodes),sem(coll_open_nodes)))

df_means_nodes_props = rename(df_means_nodes_props,nodes = `as.factor(nodes)`)
df_means_nodes_props$variable = factor(df_means_nodes_props$variable,levels=c('prop_colliders','prop_coll_open_path_0'))

#replicate with simpler plotting and grouping all data in one data.frame: by density
df_props_density = df_all_data_DAGs_nodes %>% 
  dplyr::group_by(as.factor(density_e)) %>%
  dplyr::summarise(child_p = sum(child)/n(),
                   causal_path_p = sum(causal_path)/n(),
                   dconn_p = sum(dconn)/n(),
                   adj_empty_p = sum(adj_empty)/n())

df_props_density = rename(df_props_density,density_e = `as.factor(density_e)`)
df_props_density_l = melt(df_props_density, id.vars=c("density_e"))
df_props_density_l$density_e = as.numeric(levels(df_props_density_l$density_e))[df_props_density_l$density_e]

df_means_density = df_all_data_DAGs_nodes %>% 
  dplyr::group_by(as.factor(density_e)) %>%
  dplyr::summarise(variable = c('num_adjust','edges','num_colliders','num_coll_open_path_0'),
                   means = c(mean(num_adjust),mean(edges),mean(num_colliders),mean(num_coll_open_path_0)),
                   se = c(sem(num_adjust),sem(edges),sem(num_colliders),sem(num_coll_open_path_0)))

df_means_density = rename(df_means_density,density_e = `as.factor(density_e)`)
df_means_density$variable = factor(df_means_density$variable,levels=c('num_adjust','edges','num_colliders','num_coll_open_path_0'))
df_means_density$density_e = as.numeric(levels(df_means_density$density_e))[df_means_density$density_e]

df_means_density_props = df_all_data_DAGs_nodes %>% 
  dplyr::group_by(as.factor(density)) %>%
  dplyr::summarise(variable = c('prop_colliders','prop_coll_open_path_0'),
                   means = c(mean(coll_nodes),mean(coll_open_nodes)),
                   se = c(sem(coll_nodes),sem(coll_open_nodes)))

df_means_density_props = rename(df_means_density_props,density = `as.factor(density)`)
df_means_density_props$variable = factor(df_means_density_props$variable,levels=c('prop_colliders','prop_coll_open_path_0'))
df_means_density_props$density = as.numeric(levels(df_means_density_props$density))[df_means_density_props$density]


#Both density and number of nodes

df_props_density_nodes = df_all_data_DAGs_nodes %>% 
  dplyr::group_by(as.factor(nodes),as.factor(round(density_e,1))) %>%
  dplyr::summarise(child_p = sum(child)/n(),
                   causal_path_p = sum(causal_path)/n(),
                   dconn_p = sum(dconn)/n(),
                   adj_empty_p = sum(adj_empty)/n())

df_props_density_nodes = rename(df_props_density_nodes,density_e = `as.factor(round(density_e, 1))`,nodes = `as.factor(nodes)`)
df_props_density_nodes_l = melt(df_props_density_nodes, id.vars=c('nodes',"density_e"))
df_props_density_nodes_l$density_e = as.numeric(levels(df_props_density_nodes_l$density_e))[df_props_density_nodes_l$density_e]
#df_props_density_nodes_l$density_e = round(df_props_density_nodes_l$density_e,1)

df_means_density_nodes = df_all_data_DAGs_nodes %>% 
  dplyr::group_by(as.factor(nodes),as.factor(round(density_e,1))) %>%
  dplyr::summarise(variable = c('num_adjust','edges','num_colliders','num_coll_open_path_0'),
                   means = c(mean(num_adjust),mean(edges),mean(num_colliders),mean(num_coll_open_path_0)),
                   se = c(sem(num_adjust),sem(edges),sem(num_colliders),sem(num_coll_open_path_0)))

df_means_density_nodes = rename(df_means_density_nodes,density_e = `as.factor(round(density_e, 1))`,nodes = `as.factor(nodes)`)
df_means_density_nodes$variable = factor(df_means_density_nodes$variable,levels=c('num_adjust','edges','num_colliders','num_coll_open_path_0'))
df_means_density_nodes$density_e = as.numeric(levels(df_means_density_nodes$density_e))[df_means_density_nodes$density_e]


df_means_density_nodes_props = df_all_data_DAGs_nodes %>% 
  dplyr::group_by(as.factor(nodes),as.factor(round(density_e,1))) %>%
  dplyr::summarise(variable = c('prop_colliders','prop_coll_open_path_0'), #'prop_colliders','prop_coll_open_path_0'
                   means = c(mean(coll_nodes),mean(coll_open_nodes)), #coll_nodes, coll_open_nodes
                   se = c(sem(coll_nodes),sem(coll_open_nodes))) #coll_nodes, coll_open_nodes

df_means_density_nodes_props = rename(df_means_density_nodes_props,density_e = `as.factor(round(density_e, 1))`,nodes = `as.factor(nodes)`)
df_means_density_nodes_props$variable = factor(df_means_density_nodes_props$variable,levels=c('prop_colliders','prop_coll_open_path_0'))
df_means_density_nodes_props$density_e = as.numeric(levels(df_means_density_nodes_props$density_e))[df_means_density_nodes_props$density_e]




#old code
desc_data_long = melt(desc_data, id.vars=c("num_nodes"))

desc_data_long_cont = desc_data_long[45:99,]
desc_data_long_cont$se_c = desc_data_long$value[100:154]
desc_data_long_cont_s = desc_data_long_cont[1:44,]

p_1_main = ggplot(df_props_density_l,aes(density_e,value,color = variable,shape=variable,group =variable))+ #desc_data_long[desc_data_long$variable %in% prop_names,],aes(as.factor(num_nodes),value,color = variable,shape=variable,group =variable)
  #geom_point(size=5)+
  geom_line(size=1)+
  theme_bw()+theme(axis.text.x=element_text(angle=0, size=11, vjust=0.5),axis.text.y=element_text(size = 11),axis.title.x = element_text(size = 13),axis.title.y = element_text(size = 13),legend.text = element_text(size = 12),legend.position="top")+
  scale_y_continuous(breaks=seq(0,1,.1))+coord_cartesian(ylim=c(0,1),xlim=c(min(df_props_density_l$density),max(df_props_density_l$density)))+
  scale_x_continuous(breaks=seq(min(df_props_density_l$density),max(df_props_density_l$density),.1))+
  labs(x="DAG density (normilized)", y="Proportion of sampled DAGs")+
  scale_colour_discrete(name="",    # Legend label, use darker colors
                        #breaks=c("no_expl", "with_expl"),
                        labels=c("Exposure child of outcome","Causal path","d-connected","{} in adj sets"), #"{} in adj sets"
                        l=40)+
  scale_shape_discrete(name  = "",
                       #breaks=c("no_expl", "with_expl"),
                       labels = c("Exposure child of outcome","Causal path","d-connected","{} in adj sets"))+
  facet_grid(vars(variable))+
  geom_smooth(size=.5,colour='black',alpha=.4)



p_2_main = ggplot(df_means_density,aes(density_e,means,color = variable,group =variable))+#desc_data_long_cont_s,aes(as.factor(num_nodes),value,color = variable,group =variable)
  #geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.3) +
  #geom_point(size=4,aes(shape=variable))+
  geom_line(size=1)+
  theme_bw()+theme(axis.text.x=element_text(angle=0, size=11, vjust=0.5),axis.text.y=element_text(size = 11),axis.title.x = element_text(size = 13),axis.title.y = element_text(size = 13),legend.text = element_text(size = 12),legend.position="top")+
  #scale_y_continuous(breaks=seq(0,50,5))+
  labs(x="DAG density (normilized)", y="Mean")+
  scale_colour_discrete(name="",    # Legend label, use darker colors
                        #breaks=c("no_expl", "with_expl"),
                        labels=names_x,
                        l=40)+
  scale_shape_discrete(name  = "",
                       #breaks=c("no_expl", "with_expl"),
                       labels = names_x)+
  facet_grid(vars(variable),scales = 'free')+
  #geom_smooth(aes(ymin=means-se, ymax=means+se),stat = "identity")
  geom_smooth(size=.5,colour='black',alpha=.4)
  

p_3_main = ggplot(df_means_density_props,aes(density,means,color = variable,group =variable))+#desc_data_long_cont_ss,aes(as.factor(num_nodes),prop_mean,color = variable,group =variable)
  #geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.3)+#prom_means - prom_se
  #geom_point(size=4,aes(shape=variable))+
  geom_line(size=1)+
  theme_bw()+theme(axis.text.x=element_text(angle=0, size=11, vjust=0.5),axis.text.y=element_text(size = 11),axis.title.x = element_text(size = 13),axis.title.y = element_text(size = 13),legend.text = element_text(size = 12),legend.position="top")+
  scale_y_continuous(breaks=seq(0,1,.1))+coord_cartesian(ylim=c(0,1))+
  labs(x="Density (average neighbourhood size of a node)", y="Mean proportions")+
  scale_colour_discrete(name="",    # Legend label, use darker colors
                        #breaks=c("no_expl", "with_expl"),
                        labels=names_x[3:4],
                        l=40)+
  scale_shape_discrete(name  = "",
                       #breaks=c("no_expl", "with_expl"),
                       labels = names_x[3:4])+
  #geom_smooth(aes(ymin=means-se, ymax=means+se),stat = "identity")
  geom_smooth(size=.5,colour='black',alpha=.4)

p_3_main


prop1.labels = c('Exposure child of outcome',"Causal path",'d-connected','{} in adj set')
names(prop1.labels) = c('child_p','causal_path_p','dconn_p','adj_empty_p')

prop.labels = c('Proportion nodes that are colliders',"Proportion of nodes that are 'harmful' colliders")
names(prop.labels) = c('prop_colliders','prop_coll_open_path_0')

#heatmaps


p_1_main_heat = ggplot(df_props_density_nodes_l,aes(nodes,density_e))+ #desc_data_long[desc_data_long$variable %in% prop_names,],aes(as.factor(num_nodes),value,color = variable,shape=variable,group =variable)
  #geom_point(size=5)+
  geom_tile(aes(fill = value))+
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))+
  guides(fill = guide_colourbar(title = "Proportion"))+
  theme_bw()+theme(axis.text.x=element_text(angle=0, size=11, vjust=0.5),axis.text.y=element_text(size = 11),axis.title.x = element_text(size = 13),axis.title.y = element_text(size = 13),legend.text = element_text(size = 12),legend.position="right")+
  #scale_y_continuous(breaks=seq(0,1,.1))+coord_cartesian(ylim=c(0,1),xlim=c(min(df_props_density_l$density),max(df_props_density_l$density)))+
  #scale_x_continuous(breaks=seq(min(df_props_density_l$density),max(df_props_density_l$density),.1))+
  labs(x="Number of nodes in a DAG", y="DAG density (normilized)")+
  facet_grid(vars(variable),labeller = labeller(variable = prop1.labels))

p_1_main_heat

require(gridExtra)

p_2_main_heat = df_means_density_nodes %>% group_by(variable) %>%
  do(gg = {ggplot(.,aes(nodes,density_e))+#desc_data_long_cont_s,aes(as.factor(num_nodes),value,color = variable,group =variable)
    geom_tile(aes(fill = means))+
    geom_text(aes(label = round(means, 2))) +
    scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))+
    #scale_fill_distiller(palette = "YlOrRd", direction = 1,) +
    guides(fill = guide_colourbar(title = "Mean"))+
    theme_bw()+theme(axis.text.x=element_text(angle=0, size=11, vjust=0.5),axis.text.y=element_text(size = 11),axis.title.x = element_text(size = 13),axis.title.y = element_text(size = 13),legend.text = element_text(size = 12),legend.position="right")+
    #scale_y_continuous(breaks=seq(0,50,5))+
    labs(x="Number of nodes in a DAG", y="DAG density (normilized)")+
    facet_grid(vars(variable),scales = 'fixed')})%>%
  .$gg %>% arrangeGrob(grobs = ., ncol = 1) %>% grid.arrange()
    #facet_grid(vars(variable),scales = 'free',)
  
p_2_main_heat


iris %>% group_by(Species) %>% 
  do(gg = {ggplot(., aes(Petal.Width, Petal.Length, fill = Sepal.Width)) + 
      geom_tile() + facet_grid(~Species) + 
      guides(fill = guide_colourbar(title.position = "top")) +
      theme(legend.position = "top")}) %>% 
  .$gg %>% arrangeGrob(grobs = ., nrow = 1) %>% grid.arrange()

p_3_main_heat = ggplot(df_means_density_nodes_props,aes(nodes,density_e))+#desc_data_long_cont_ss,aes(as.factor(num_nodes),prop_mean,color = variable,group =variable)
  geom_tile(aes(fill = means))+
  geom_text(aes(label = round(means, 1))) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))+
  guides(fill = guide_colourbar(title = "Mean\nproportion"))+
  theme_bw()+theme(axis.text.x=element_text(angle=0, size=11, vjust=0.5),axis.text.y=element_text(size = 11),axis.title.x = element_text(size = 13),axis.title.y = element_text(size = 13),
                   legend.text = element_text(size = 12),legend.position="right")+
  #scale_y_continuous(breaks=seq(0,1,.1))+coord_cartesian(ylim=c(0,1))+
  labs(x="Number of nodes in a DAG", y="DAG density (normalized)")+
  facet_grid(vars(variable),labeller = labeller(variable=prop.labels))

p_3_main_heat


desc_data_long_cont_ss = desc_data_long_cont_s[23:44,]
desc_data_long_cont_ss$prop_mean = desc_data_long_cont_ss$value/desc_data_long_cont_ss$num_nodes
desc_data_long_cont_ss$prop_se = desc_data_long_cont_ss$se_c*(1/desc_data_long_cont_ss$num_nodes)# the following is wrong: sqrt(desc_data_long_cont_ss$num_nodes)


####save plots #######
ggsave("heat_map_cont_data_plot_norm.pdf",p_2_main_heat, width = 25, height = 20, units = "cm",
        path = "/Users/marko/Dropbox/Apps/Overleaf/Causation_corrleation_workshop_NeurIPS2021")


g = make_tree(12)
plot.igraph(g)
edge_density(g)
edge_density(as.undirected(g))
is_dag(g)
remotes::install_github("MiloMonnier/supplynet")
library(supplynet)
dagDensity(g)



((n-1)*(n))/2 #maxim # of edges in a dag



######## Finding distributions that best fit data ##########

vec_dist = function(indx,df_counting_plot){
  
  n_e = as.numeric(df_counting_plot$edges[indx])
  n_e_vec =rep(n_e,df_counting_plot$num_adjust_sets[indx])
  
}


list_dist = sapply(seq_len(nrow(df_counting_plot)), vec_dist,df_counting_plot=df_counting_plot)

vec_d = unlist(list_dist)

par(mfrow=c(1,1))

library(MASS)
fit_norm = fitdistr(vec_d, densfun="normal")

hist(vec_d, pch=20, breaks=25, prob=TRUE, main="")
curve(dnorm(x, fit_norm$estimate[1], fit_norm$estimate[2]), col="red", lwd=2, add=T)



descdist(vec_d, discrete=TRUE, boot=5000)

library(fitdistrplus)
fit_n <- fitdist(vec_d, "norm")
cdfcomp(fit_n, xlogscale = TRUE, ylogscale = TRUE)

fit_p  <- fitdist(vec_d, "pois")
fit_nb  <- fitdist(vec_d, "nbinom")

par(mfrow=c(2,2))
plot.legend <- c("Normal", "Poisson", "Negative binomial")
denscomp(list(fit_n, fit_p, fit_nb), legendtext = plot.legend)
cdfcomp (list(fit_n, fit_p, fit_nb), legendtext = plot.legend)
qqcomp  (list(fit_n, fit_p, fit_nb), legendtext = plot.legend)
ppcomp  (list(fit_n, fit_p, fit_nb), legendtext = plot.legend)

par(mfrow=c(1,1))
library(actuar)
fit_ll <- fitdist(vec_d, "llogis", start = list(shape = 1, scale = 500))
#fit_P  <- fitdist(vec_d, "pareto", start = list(shape = 1, scale = 500))
fit_B  <- fitdist(vec_d, "burr",   start = list(shape1 = 0.3, shape2 = 1, rate = 1))
cdfcomp(list(fit_ln, fit_ll, fit_B), xlogscale = TRUE, ylogscale = TRUE,
        legendtext = c("lognormal", "loglogistic", "Burr"), lwd=2)

gofstat(list(fit_n, fit_p, fit_nb), fitnames = c("norm", "pois", "nbinom"))
