from functools import lru_cache 

a = [2,4,1,2,1,3,1,1,5]

def frog(swamp): 
    
    @lru_cache(maxsize=None)
    def jumper(k):
        starting_energy = swamp[k]
        # Enough energy to go all the way: 
        if starting_energy >= len(swamp[k:]): 
            return 1
        # Jumps in the swamp:
        else:
            possible_energies = []
            for i in range (1, starting_energy + 1):  
                possible_energies.append(starting_energy - i + swamp[i])
            desired_circumstances = []
            for j in possible_energies: 
                desired_circumstances.append(j - len(swamp[j:]))    
            starting_energy = max(desired_circumstances)
            return 1

    # Call function     
    return jumper(0)


            

