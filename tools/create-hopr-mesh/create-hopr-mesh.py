import numpy as np
from timeit import default_timer as timer
import argparse
import re
import shutil
import os
import grp

import pwd
from pwd import getpwuid

# Bind raw_input to input in Python 2
try:
    input = raw_input
except NameError:
    pass

def WriteArrayToHDF5(f1, Array, ArrayName, dtype):
    """Writes an array to .h5

    :Array: TODO
    :returns: TODO

    """
    print(Array)
    dset = f1.create_dataset(ArrayName, shape=Array.shape, dtype=dtype)
    if not Array.any() :
        print(" %s has dimension %s. Skipping" % (dset,Array.shape))
        pass
    else:
        dset.write_direct(np.ascontiguousarray(Array))

def CreateFile(h5_output_file,args):

    nLostParts = 1

    # Read std.out.lost file with all lost particles and write the data to a .h5 file
    if nLostParts > 0:

        # 2.1. Open .h5 file
        f1 = h5py.File(h5_output_file,'w')

        # 2.7. Write attributes
        f1.attrs.create('Ngeo'         , [1]   , None , dtype='i4')
        f1.attrs.modify('Version'      , [1.0])   # these brackets [] are required for ParaView plugin !
        f1.attrs.create('nBCs'         , [6]   , None , dtype='i4')
        f1.attrs.create('nElems'       , [2]   , None , dtype='i4')
        f1.attrs.create('nNodes'       , [16]  , None , dtype='i4')
        f1.attrs.create('nEdges'       , [2*12], None , dtype='i4')
        f1.attrs.create('nSides'       , [12]  , None , dtype='i4')
        f1.attrs.create('nVertices'    , [2*8] , None , dtype='i4')
        f1.attrs.create('nUniqueNodes' , [12]  , None , dtype='i4')
        f1.attrs.create('nUniqueSides' , [6]   , None , dtype='i4')
        f1.attrs.create('nUniqueEdges' , [20]  , None , dtype='i4')
        f1.attrs.create('nFEMEdges'    , [6]   , None , dtype='i4')
        f1.attrs.create('nFEMVertices' , [2]   , None , dtype='i4')


        # HOPR:    String, length = 255, padding = H5T_STR_SPACEPAD, cset = H5T_CSET_ASCII
        # CURRENT: String, length = 255, padding = H5T_STR_NULLPAD , cset = H5T_CSET_ASCII

        #BCNames = [b'BC_periodicx+'.ljust(255)]
        BCNames = ['BC_periodicx+',\
                   'BC_periodicx-',\
                   'BC_periodicy+',\
                   'BC_periodicy-',\
                   'BC_periodicz+',\
                   'BC_periodicz-'\
                   ]
        print(BCNames)
        f1.create_dataset('BCNames',(len(BCNames)),'S255',BCNames)


        BCType = np.zeros((6,4))
        BCType[0 , :] = [1 , 0 , 0 ,  1]
        BCType[1 , :] = [1 , 0 , 0 , -1]
        BCType[2 , :] = [1 , 0 , 0 ,  2]
        BCType[3 , :] = [1 , 0 , 0 , -2]
        BCType[4 , :] = [1 , 0 , 0 ,  3]
        BCType[5 , :] = [1 , 0 , 0 , -3]
        WriteArrayToHDF5(f1, BCType,'BCType',np.int32)


        ElemBarycenters = np.zeros((2,3))
        ElemBarycenters[0  , :] = [0.5 , 0.5 , 0.5]
        ElemBarycenters[1  , :] = [1.5 , 0.5 , 0.5]
        WriteArrayToHDF5(f1, ElemBarycenters,'ElemBarycenters',np.float64)


        ElemCounter = np.zeros((11,2))
        ElemCounter[0 , :] = [104,0]
        ElemCounter[1 , :] = [204,0]
        ElemCounter[2 , :] = [105,0]
        ElemCounter[3 , :] = [115,0]
        ElemCounter[4 , :] = [205,0]
        ElemCounter[5 , :] = [106,0]
        ElemCounter[6 , :] = [116,0]
        ElemCounter[7 , :] = [206,0]
        ElemCounter[8 , :] = [108,2]
        ElemCounter[9 , :] = [118,0]
        ElemCounter[10, :] = [208,0]
        WriteArrayToHDF5(f1, ElemCounter,'ElemCounter',np.int32)


        ElemInfo = np.zeros((2,10))
        #          , Element Type , Zone , offsetIndSIDE , lastIndSIDE , offsetIndNODE , lastIndNODE , offsetIndEDGE , lastIndEDGE , offsetIndVERTEX , lastIndVERTEX
        ElemInfo[0 , :] = [108    , 1    , 0             , 6           , 0             , 8           , 0             , 12          , 0               , 8            ]
        ElemInfo[1 , :] = [108    , 1    , 6             , 12          , 8             , 16          , 12            , 24          , 8               , 16           ]
        WriteArrayToHDF5(f1, ElemInfo,'ElemInfo',np.int32)



        ElemWeight = np.zeros((2,1))
        ElemWeight[0  , :] = [1.0]
        ElemWeight[1  , :] = [1.0]
        WriteArrayToHDF5(f1, ElemWeight,'ElemWeight',np.float64)


        GlobalNodeIDs = np.zeros((16,1))
        GlobalNodeIDs[0  , :] = [1]
        GlobalNodeIDs[1  , :] = [2]
        GlobalNodeIDs[2  , :] = [4]
        GlobalNodeIDs[3  , :] = [3]
        GlobalNodeIDs[4  , :] = [5]
        GlobalNodeIDs[5  , :] = [6]
        GlobalNodeIDs[6  , :] = [8]
        GlobalNodeIDs[7  , :] = [7]
        GlobalNodeIDs[8  , :] = [2]
        GlobalNodeIDs[9  , :] = [9]
        GlobalNodeIDs[10 , :] = [3]
        GlobalNodeIDs[11 , :] = [10]
        GlobalNodeIDs[12 , :] = [6]
        GlobalNodeIDs[13 , :] = [11]
        GlobalNodeIDs[14 , :] = [7]
        GlobalNodeIDs[15 , :] = [12]
        WriteArrayToHDF5(f1, GlobalNodeIDs,'GlobalNodeIDs',np.int32)



        NodeCoords = np.zeros((16,3))
        NodeCoords[0,:]  = [0.0 , 0.0 , 0.0]
        NodeCoords[1,:]  = [1.0 , 0.0 , 0.0]
        NodeCoords[2,:]  = [0.0 , 1.0 , 0.0]
        NodeCoords[3,:]  = [1.0 , 1.0 , 0.0]
        NodeCoords[4,:]  = [0.0 , 0.0 , 1.0]
        NodeCoords[5,:]  = [1.0 , 0.0 , 1.0]
        NodeCoords[6,:]  = [0.0 , 1.0 , 1.0]
        NodeCoords[7,:]  = [1.0 , 1.0 , 1.0]
        NodeCoords[8,:]  = [1.0 , 0.0 , 0.0]
        NodeCoords[9,:]  = [2.0 , 0.0 , 0.0]
        NodeCoords[10,:] = [1.0 , 1.0 , 0.0]
        NodeCoords[11,:] = [2.0 , 1.0 , 0.0]
        NodeCoords[12,:] = [1.0 , 0.0 , 1.0]
        NodeCoords[13,:] = [2.0 , 0.0 , 1.0]
        NodeCoords[14,:] = [1.0 , 1.0 , 1.0]
        NodeCoords[15,:] = [2.0 , 1.0 , 1.0]
        WriteArrayToHDF5(f1, NodeCoords,'NodeCoords',np.float64)



        SideInfo = np.zeros((12,5))
        SideInfo[0  , :] = [4 , -1 , 1 , 61 , 5]
        SideInfo[1  , :] = [4 , -2 , 1 , 42 , 3]
        SideInfo[2  , :] = [4 , -3 , 2 , 51 , 0]
        SideInfo[3  , :] = [4 , 2  , 1 , 22 , 4]
        SideInfo[4  , :] = [4 , -4 , 2 , 31 , 1]
        SideInfo[5  , :] = [4 , 1  , 1 , 11 , 6]
        SideInfo[6  , :] = [4 , -5 , 2 , 61 , 5]
        SideInfo[7  , :] = [4 , -6 , 2 , 42 , 3]
        SideInfo[8  , :] = [4 , 4  , 1 , 51 , 2]
        SideInfo[9  , :] = [4 , 6  , 2 , 22 , 4]
        SideInfo[10 , :] = [4 , 3  , 1 , 31 , 0]
        SideInfo[11 , :] = [4 , 5  , 2 , 11 , 6]
        WriteArrayToHDF5(f1, SideInfo,'SideInfo',np.int32)













        EdgeInfo = np.zeros((24,3))
        EdgeInfo[0  , :] = [  1 , 0  , 3 ]
        EdgeInfo[1  , :] = [  2 , 3  , 6 ]
        EdgeInfo[2  , :] = [ -1 , 6  , 9 ]
        EdgeInfo[3  , :] = [ -3 , 9  , 12]
        EdgeInfo[4  , :] = [  4 , 12 , 15]
        EdgeInfo[5  , :] = [  5 , 15 , 18]
        EdgeInfo[6  , :] = [  5 , 18 , 21]
        EdgeInfo[7  , :] = [  4 , 21 , 24]
        EdgeInfo[8  , :] = [  1 , 24 , 27]
        EdgeInfo[9  , :] = [  2 , 27 , 30]
        EdgeInfo[10 , :] = [ -1 , 30 , 33]
        EdgeInfo[11 , :] = [ -3 , 33 , 36]

        EdgeInfo[12 , :] = [  6 , 36 , 39]
        EdgeInfo[13 , :] = [  3 , 39 , 42]
        EdgeInfo[14 , :] = [ -6 , 42 , 45]
        EdgeInfo[15 , :] = [ -2 , 45 , 48]
        EdgeInfo[16 , :] = [  5 , 48 , 51]
        EdgeInfo[17 , :] = [  4 , 51 , 54]
        EdgeInfo[18 , :] = [  4 , 54 , 57]
        EdgeInfo[19 , :] = [  5 , 57 , 60]
        EdgeInfo[20 , :] = [  6 , 60 , 63]
        EdgeInfo[21 , :] = [  3 , 63 , 66]
        EdgeInfo[22 , :] = [ -6 , 66 , 69]
        EdgeInfo[23 , :] = [ -2 , 79 , 72]

        WriteArrayToHDF5(f1, EdgeInfo,'EdgeInfo',np.int32)


        EdgeConnectInfo = np.zeros((72,2))
        EdgeConnectInfo[0  , : ] = [ -1 , 9   ] # 1
        EdgeConnectInfo[1  , : ] = [ -1 , -11 ]
        EdgeConnectInfo[2  , : ] = [ -1 , -3  ]
        EdgeConnectInfo[3  , : ] = [ -1 , 10  ] # 2
        EdgeConnectInfo[4  , : ] = [ -2 , -4  ]
        EdgeConnectInfo[5  , : ] = [ -2 , -12 ]
        EdgeConnectInfo[6  , : ] = [ -1 , -9  ] # 1
        EdgeConnectInfo[7  , : ] = [ -1 , 11  ]
        EdgeConnectInfo[8  , : ] = [  1 , -1  ]
        EdgeConnectInfo[9  , : ] = [ -1 , 12  ] # 3
        EdgeConnectInfo[10 , : ] = [  2 , -2  ]
        EdgeConnectInfo[11 , : ] = [ -2 , -10 ]

        EdgeConnectInfo[12 , : ] = [ -1 , 8   ] # 4
        EdgeConnectInfo[13 , : ] = [ -2 , 6   ]
        EdgeConnectInfo[14 , : ] = [ -2 , 7   ]
        EdgeConnectInfo[15 , : ] = [ -1 , 7   ] # 5
        EdgeConnectInfo[16 , : ] = [ -2 , 5   ]
        EdgeConnectInfo[17 , : ] = [ -2 , 8   ]
        EdgeConnectInfo[18 , : ] = [  1 , 6   ] # 5
        EdgeConnectInfo[19 , : ] = [ -2 , 5   ]
        EdgeConnectInfo[20 , : ] = [ -2 , 8   ]
        EdgeConnectInfo[21 , : ] = [  1 , 5   ] # 4
        EdgeConnectInfo[22 , : ] = [ -2 , 6   ]
        EdgeConnectInfo[23 , : ] = [ -2 , 7   ]

        EdgeConnectInfo[24 , : ] = [ -1 , 1   ] # 1
        EdgeConnectInfo[25 , : ] = [ -1 , -11 ]
        EdgeConnectInfo[26 , : ] = [ -1 , -3  ]
        EdgeConnectInfo[27 , : ] = [  1 , 2   ] # 2
        EdgeConnectInfo[28 , : ] = [ -2 , -4  ]
        EdgeConnectInfo[29 , : ] = [ -2 , -12 ]
        EdgeConnectInfo[30 , : ] = [ -1 , -9  ] # 1
        EdgeConnectInfo[31 , : ] = [ -1 , 3   ]
        EdgeConnectInfo[32 , : ] = [  1 , -1  ]
        EdgeConnectInfo[33 , : ] = [ -1 , 4   ] # 3
        EdgeConnectInfo[34 , : ] = [ -2 , -2  ]
        EdgeConnectInfo[35 , : ] = [ -2 , -10 ]






        EdgeConnectInfo[36 , :] = [ -2 , 9   ] # 6
        EdgeConnectInfo[37 , :] = [ -2 , -11 ]
        EdgeConnectInfo[38 , :] = [ -2 , -3  ]
        EdgeConnectInfo[39 , :] = [ -2 , 10  ] # 3
        EdgeConnectInfo[40 , :] = [  1 , -4  ]
        EdgeConnectInfo[41 , :] = [ -1 , -12 ]
        EdgeConnectInfo[42 , :] = [ -2 , -9  ] # 6
        EdgeConnectInfo[43 , :] = [ -2 , 11  ]
        EdgeConnectInfo[44 , :] = [  2 , -1  ]
        EdgeConnectInfo[45 , :] = [ -2 , 12  ] # 2
        EdgeConnectInfo[46 , :] = [  1 , -2  ]
        EdgeConnectInfo[47 , :] = [ -1 , -10 ]

        EdgeConnectInfo[48 , :] = [ -2 , 8   ] # 4
        EdgeConnectInfo[49 , :] = [  1 , 6   ]
        EdgeConnectInfo[50 , :] = [ -1 , 7   ]
        EdgeConnectInfo[51 , :] = [ -2 , 7   ] # 5
        EdgeConnectInfo[52 , :] = [  1 , 5   ]
        EdgeConnectInfo[53 , :] = [ -1 , 8   ]
        EdgeConnectInfo[54 , :] = [ -2 , 6   ] # 5
        EdgeConnectInfo[55 , :] = [  1 , 5   ]
        EdgeConnectInfo[56 , :] = [ -1 , 8   ]
        EdgeConnectInfo[57 , :] = [ -2 , 5   ] # 4
        EdgeConnectInfo[58 , :] = [  1 , 6   ]
        EdgeConnectInfo[59 , :] = [ -1 , 7   ]

        EdgeConnectInfo[60 , :] = [  2 , 1   ] # 1
        EdgeConnectInfo[61 , :] = [ -2 , -11 ]
        EdgeConnectInfo[62 , :] = [ -2 , -3  ]
        EdgeConnectInfo[63 , :] = [ -2 , 2   ] # 2
        EdgeConnectInfo[64 , :] = [  1 , -4  ]
        EdgeConnectInfo[65 , :] = [ -1 , -12 ]
        EdgeConnectInfo[66 , :] = [ -2 , -9  ] # 1
        EdgeConnectInfo[67 , :] = [ -2 , 3   ]
        EdgeConnectInfo[68 , :] = [  2 , -1  ]
        EdgeConnectInfo[69 , :] = [ -2 , 4   ] # 3
        EdgeConnectInfo[70 , :] = [  1 , -2  ]
        EdgeConnectInfo[71 , :] = [ -1 , -10 ]

        WriteArrayToHDF5(f1, EdgeConnectInfo,'EdgeConnectInfo',np.int32)









        VertexInfo = np.zeros((16,3))
        VertexInfo[0 , :] = [1 ,  0 , 7  ]
        VertexInfo[1 , :] = [2 ,  7 , 14 ]
        VertexInfo[2 , :] = [2 , 14 , 21 ]
        VertexInfo[3 , :] = [1 , 21 , 28 ]

        VertexInfo[4 , :] = [1 , 28 , 35 ]
        VertexInfo[5 , :] = [2 , 35 , 42 ]
        VertexInfo[6 , :] = [2 , 42 , 49 ]
        VertexInfo[7 , :] = [1 , 49 , 56 ]

        VertexInfo[8 , :] = [2 , 56 , 63 ]
        VertexInfo[9 , :] = [1 , 63 , 70 ]
        VertexInfo[10, :] = [1 , 70 , 77 ]
        VertexInfo[11, :] = [2 , 77 , 84 ]

        VertexInfo[12, :] = [2 , 84 , 91 ]
        VertexInfo[13, :] = [1 , 91 , 98 ]
        VertexInfo[14, :] = [1 , 98 , 105]
        VertexInfo[15, :] = [2 , 105, 112]
        WriteArrayToHDF5(f1, VertexInfo,'VertexInfo',np.int32)


        # This is not sorted!
        # 1/1 and 1/2 are master, the rest is slave
        VertexConnectInfo = np.zeros((112,2))
        VertexConnectInfo[0   , :] = [-1 , 4] # 1
        VertexConnectInfo[1   , :] = [-1 , 5]
        VertexConnectInfo[2   , :] = [-1 , 8]
        VertexConnectInfo[3   , :] = [-2 , 2]
        VertexConnectInfo[4   , :] = [-2 , 3]
        VertexConnectInfo[5   , :] = [-2 , 6]
        VertexConnectInfo[6   , :] = [-2 , 7]

        VertexConnectInfo[7   , :] = [-1 , 3] # 2
        VertexConnectInfo[8   , :] = [-1 , 6]
        VertexConnectInfo[9   , :] = [-1 , 7]
        VertexConnectInfo[10  , :] = [-2 , 1]
        VertexConnectInfo[11  , :] = [-2 , 4]
        VertexConnectInfo[12  , :] = [-2 , 5]
        VertexConnectInfo[13  , :] = [-2 , 8]

        VertexConnectInfo[14  , :] = [ 1 , 2] # 2
        VertexConnectInfo[15  , :] = [-1 , 6]
        VertexConnectInfo[16  , :] = [-1 , 7]
        VertexConnectInfo[17  , :] = [-2 , 1]
        VertexConnectInfo[18  , :] = [-2 , 4]
        VertexConnectInfo[19  , :] = [-2 , 5]
        VertexConnectInfo[20  , :] = [-2 , 8]

        VertexConnectInfo[21  , :] = [ 1 , 1] # 1
        VertexConnectInfo[22  , :] = [-1 , 5]
        VertexConnectInfo[23  , :] = [-1 , 8]
        VertexConnectInfo[24  , :] = [-2 , 2]
        VertexConnectInfo[25  , :] = [-2 , 3]
        VertexConnectInfo[26  , :] = [-2 , 6]
        VertexConnectInfo[27  , :] = [-2 , 7]

       # -----------------

        VertexConnectInfo[28  , :] = [ 1 , 1] # 1
        VertexConnectInfo[29  , :] = [-1 , 4]
        VertexConnectInfo[30  , :] = [-1 , 8]
        VertexConnectInfo[31  , :] = [-2 , 2]
        VertexConnectInfo[32  , :] = [-2 , 3]
        VertexConnectInfo[33  , :] = [-2 , 6]
        VertexConnectInfo[34  , :] = [-2 , 7]

        VertexConnectInfo[35  , :] = [-1 , 3] # 2
        VertexConnectInfo[36  , :] = [ 1 , 2]
        VertexConnectInfo[37  , :] = [-1 , 7]
        VertexConnectInfo[38  , :] = [-2 , 1]
        VertexConnectInfo[39  , :] = [-2 , 4]
        VertexConnectInfo[40  , :] = [-2 , 5]
        VertexConnectInfo[41  , :] = [-2 , 8]

        VertexConnectInfo[42  , :] = [-1 , 3] # 2
        VertexConnectInfo[43  , :] = [ 1 , 2]
        VertexConnectInfo[44  , :] = [-1 , 6]
        VertexConnectInfo[45  , :] = [-2 , 1]
        VertexConnectInfo[46  , :] = [-2 , 4]
        VertexConnectInfo[47  , :] = [-2 , 5]
        VertexConnectInfo[48  , :] = [-2 , 8]

        VertexConnectInfo[49  , :] = [ 1 , 1] # 1
        VertexConnectInfo[50  , :] = [-1 , 5]
        VertexConnectInfo[51  , :] = [-1 , 4]
        VertexConnectInfo[52  , :] = [-2 , 2]
        VertexConnectInfo[53  , :] = [-2 , 3]
        VertexConnectInfo[54  , :] = [-2 , 6]
        VertexConnectInfo[55  , :] = [-2 , 7]

       # -----------------

        VertexConnectInfo[56  , :] = [-1 , 3] # 2
        VertexConnectInfo[57  , :] = [ 1 , 2]
        VertexConnectInfo[58  , :] = [-1 , 7]
        VertexConnectInfo[59  , :] = [-1 , 6]
        VertexConnectInfo[60  , :] = [-2 , 4]
        VertexConnectInfo[61  , :] = [-2 , 5]
        VertexConnectInfo[62  , :] = [-2 , 8]

        VertexConnectInfo[63  , :] = [ 1 , 1] # 1
        VertexConnectInfo[64  , :] = [-1 , 5]
        VertexConnectInfo[65  , :] = [-1 , 4]
        VertexConnectInfo[66  , :] = [-1 , 8]
        VertexConnectInfo[67  , :] = [-2 , 3]
        VertexConnectInfo[68  , :] = [-2 , 6]
        VertexConnectInfo[69  , :] = [-2 , 7]

        VertexConnectInfo[70  , :] = [ 1 , 1] # 1
        VertexConnectInfo[71  , :] = [-1 , 5]
        VertexConnectInfo[72  , :] = [-1 , 4]
        VertexConnectInfo[73  , :] = [-1 , 8]
        VertexConnectInfo[74  , :] = [-2 , 2]
        VertexConnectInfo[75  , :] = [-2 , 6]
        VertexConnectInfo[76  , :] = [-2 , 7]

        VertexConnectInfo[77  , :] = [-1 , 3] # 2
        VertexConnectInfo[78  , :] = [ 1 , 2]
        VertexConnectInfo[79  , :] = [-1 , 7]
        VertexConnectInfo[80  , :] = [-1 , 6]
        VertexConnectInfo[81  , :] = [-2 , 8]
        VertexConnectInfo[82  , :] = [-2 , 5]
        VertexConnectInfo[83  , :] = [-2 , 1]

       # -----------------

        VertexConnectInfo[84  , :] = [-1 , 3] # 2
        VertexConnectInfo[85  , :] = [ 1 , 2]
        VertexConnectInfo[86  , :] = [-1 , 7]
        VertexConnectInfo[87  , :] = [-1 , 6]
        VertexConnectInfo[88  , :] = [-2 , 4]
        VertexConnectInfo[89  , :] = [-2 , 1]
        VertexConnectInfo[90  , :] = [-2 , 8]

        VertexConnectInfo[91  , :] = [ 1 , 1] # 1
        VertexConnectInfo[92  , :] = [-1 , 5]
        VertexConnectInfo[93  , :] = [-1 , 4]
        VertexConnectInfo[94  , :] = [-1 , 8]
        VertexConnectInfo[95  , :] = [-2 , 3]
        VertexConnectInfo[96  , :] = [-2 , 2]
        VertexConnectInfo[97  , :] = [-2 , 7]

        VertexConnectInfo[98  , :] = [ 1 , 1] # 1
        VertexConnectInfo[99  , :] = [-1 , 5]
        VertexConnectInfo[100 , :] = [-1 , 4]
        VertexConnectInfo[101 , :] = [-1 , 8]
        VertexConnectInfo[102 , :] = [-2 , 2]
        VertexConnectInfo[103 , :] = [-2 , 6]
        VertexConnectInfo[104 , :] = [-2 , 3]

        VertexConnectInfo[105 , :] = [-1 , 3] # 2
        VertexConnectInfo[106 , :] = [ 1 , 2]
        VertexConnectInfo[107 , :] = [-1 , 7]
        VertexConnectInfo[108 , :] = [-1 , 6]
        VertexConnectInfo[109 , :] = [-2 , 4]
        VertexConnectInfo[110 , :] = [-2 , 5]
        VertexConnectInfo[111 , :] = [-2 , 1]




        WriteArrayToHDF5(f1, VertexConnectInfo,'VertexConnectInfo',np.int32)







#
        # 2.8. Close .h5 data file
        f1.close()



class bcolors :
    """color and font style definitions for changing output appearance"""
    # Reset (user after applying a color to return to normal coloring)
    ENDC   ='\033[0m'

    # Regular Colors
    BLACK  ='\033[0;30m'
    RED    ='\033[0;31m'
    GREEN  ='\033[0;32m'
    YELLOW ='\033[0;33m'
    BLUE   ='\033[0;34m'
    PURPLE ='\033[0;35m'
    CYAN   ='\033[0;36m'
    WHITE  ='\033[0;37m'

    # Text Style
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

def red(text) :
    return bcolors.RED+text+bcolors.ENDC

def green(text) :
    return bcolors.GREEN+text+bcolors.ENDC

def blue(text) :
    return bcolors.BLUE+text+bcolors.ENDC

def yellow(text) :
    return bcolors.YELLOW+text+bcolors.ENDC

# import h5 I/O routines
try :
    import h5py
    h5py_module_loaded = True
except ImportError :
    print(red('Could not import h5py module. This is required for handling .h5 files.'))
    exit(0)

# Start the timer
start = timer()

"""get command line arguments"""
parser = argparse.ArgumentParser(description='DESCRIPTION:\nTool for creating .h5 mesh files in the HOPR format.', formatter_class=argparse.RawTextHelpFormatter)
#parser.add_argument('files', type=str, help='Files (std*.out) that are to be cleaned.', nargs='+')
parser.add_argument('-d', '--debug', action='store_true', help='Print additional information regarding the files onto screen.')
#parser.add_argument('-s', '--save', action='store_true', help='Save *.bak backup files to see what was actually removed from the std-x.out files.')
#parser.add_argument('-i', '--iter', action='store_false', help='Do not remove lines matching "iter:   702 time:   3.2151600000000616E-008" (default=False).')

# Get command line arguments
args = parser.parse_args()

# Display all command line arguments
print('='*132)
print("Running with the following command line options")
for arg in list(args.__dict__) :
    print(arg.ljust(15)+" = [ "+str(getattr(args,arg))+" ]")
print('='*132)

CreateFile('python-mesh.h5',args)

print(132*"-")
