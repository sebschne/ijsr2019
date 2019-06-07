import numpy as np


def arg_max_all(vec):

    max_val = 0
    max_ind = []
    if vec is None:
        return None
    for i in range(0,len(vec)):
        if vec[i] > max_val:
            max_val = vec[i]
    for i in range(0,len(vec)):
        if vec[i] == max_val:
            max_ind.append(i)
    return max_ind


def arg_max_index(vec):

    mv = vec[0]
    for i in range(0,len(vec)):
        if vec[i]>mv:
            mv = vec[i]
    return mv

"""Return the Hamming distance between equal-length sequences"""
def hammingDistance(s1, s2,print_out = True):
    if print_out:
        print('##### Comparing String ####')
        print ("target:  " ,s1)
        print ("learned: " ,s2)

    if len(s1) != len(s2):
        raise ValueError("Undefined for sequences of unequal length")
    return sum(el1 != el2 for el1, el2 in zip(s1, s2))

""""Return the discounted position error"""
def discount_error(s1,s2, print_out = True):
    if print_out:
        print('##### Comparing String ####')
        print ("target:  " ,s1)
        print ("learned: " ,s2)

    error = 0
    for i in s1:
        #print "whats going on?" , (ggs1.index(i)+1)
        divisor = s1.index(i)+2
        w = 1.0/np.log(divisor)
        error += w * np.abs(s1.index(i)-s2.index(i))
    if print_out:
        print ("#### Discounted Error: ", error, " ####")
    return error

def position_error(s1,s2, print_out = True):
    if print_out:
        print('##### Comparing String ####')
        print ("target: " ,s1)
        print ('learned: ' ,s2)
        print ("#### Position error: ", s2.index(s1[0]), " ####")
    return s2.index(s1[0])


def argmax_given_ind_set(vec, ind_set):

    if len(ind_set) == 0:
        return -1
    max_val = vec[ind_set[0]]
    for i in range(0,len(ind_set)):
        if vec[ind_set[i]] > max_val:
            max_val = vec[ind_set[i]]

    max_ind=[]
    for i in range(0,len(ind_set)):
        if vec[ind_set[i]] == max_val:
            max_ind.append(ind_set[i])
    return max_ind[np.random.random_integers(0, len(max_ind)-1)] #todo check the random output

def kl(p,q):
    v = 0.0
    if (q>=1.0 or q<0.0):
        print "Error: q must be in (0,1)"
        exit(0)
    if p>0: v += p * np.log(p/q)
    if p<1: v+=(1-p)*np.log((1-p)/(1-q))
    return v

