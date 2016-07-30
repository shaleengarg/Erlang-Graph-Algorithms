
This project is done as a summer project in IIIT-H in Distributed Systems (CVEST)
----
It implements a synchronous Breadth First Spanning Tree (directed graph)on more than one machine  
Inputs are :  
1. A file which has information of the graph ie number of nodes and connected nodes for each of the respective nodes  
Example ->  

3. %%Is the number of nodes   
[2, 3]. %%is the list of nodes connected to the first node  
[1, 3]. %%Is the list of nodes connected to the second node  
[1, 2]. %%Is the list of nodes connected to the third node  

2. A file which has names of all the machines available  
Example ->  

'M1@envy'.  
'M2@envy'.  
'M3@envy'.  

Implementation:  
---
The algorithm chooses machines at random to spawn a process which represents a node in the graph   
The first node is inherently made the root node. This process starts the process of making the tree.   

Answer: 
---
It returns a list of tuples of the form {Parent, node}  
The root node (1) returns tuple of the form {0, 1} as there is no parent to the root node   

References used:  
---
1. Programming Erlang - by Joe Armstrong  
2. Distributed Computing - by Ajay D. Kshemkalyani  

Contact:  
---
Email: shaleengarg.in@gmail.com   
