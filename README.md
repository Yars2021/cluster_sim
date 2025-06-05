```bash
chmod +x ./cluster_sim.sh

cluster_sim.sh help
cluster_sim.sh clean
cluster_sim.sh "[{single_cast},{gossip}]" 10 3 2 10 7
cluster_sim.sh "[{single_cast},{gossip},{multicast,2},{broadcast}]" 100 15 5 1 75
```