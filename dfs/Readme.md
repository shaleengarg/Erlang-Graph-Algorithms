Asynchronous concurrent-initiator depth first search spanning tree
===

Input 
---
The code takes input of a graph data file of the form
3. %%Number of nodes  
[2, 3].  %%1st node is connected to 2 and 3  
[1, 3].  %%2nd node is connected to 1 and 3  
[1, 2].  %%3rd node is connected to 1 and 2  

It also takes input of the machines it has in a file  
't1@home'.  
't2@home'.  

Call  
---
$erl -sname t1 -setcookie abc ##It names one machine t1, init all the machines like so
(t1@home)1>dfs:execute('graph.txt', 'computer.txt').

Output  
---
The code will output occasional status and finally return children of every node
