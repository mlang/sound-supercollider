{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Sound.SuperCollider.SynthDef.Builtin (SynthDef, def, playBuf) where

import Sound.SuperCollider.SynthDef

def :: SynthDef
def = SynthDef
  [ GraphDef
    "default"
    [-0.4,0.0,0.4,0.3,4000.0,5000.0,2500.0,3200.0,1.0,1.0e-2,0.7,2.0]
    [0.0,440.0,0.1,0.0,1.0]
    [("out",0),("freq",1),("amp",2),("pan",3),("gate",4)]
    [ UGen "Control" Scalar 0 [] [Scalar]
    , UGen "Control" Control 1 [] [Control,Control,Control,Control]
    , UGen "VarSaw" Audio 0 [(1,0),(-1,1),(-1,3)] [Audio]
    , UGen "BinaryOpUGen" Audio 2 [(2,0),(-1,3)] [Audio]
    , UGen "Linen" Control 0 [(1,3),(-1,9),(-1,10),(-1,3),(-1,11)] [Control]
    , UGen "Rand" Scalar 0 [(-1,0),(-1,1)] [Scalar]
    , UGen "BinaryOpUGen" Control 0 [(1,0),(5,0)] [Control]
    , UGen "VarSaw" Audio 0 [(6,0),(-1,1),(-1,3)] [Audio]
    , UGen "BinaryOpUGen" Audio 2 [(7,0),(-1,3)] [Audio]
    , UGen "Rand" Scalar 0 [(-1,1),(-1,2)] [Scalar]
    , UGen "BinaryOpUGen" Control 0 [(1,0),(9,0)] [Control]
    , UGen "VarSaw" Audio 0 [(10,0),(-1,1),(-1,3)] [Audio]
    , UGen "BinaryOpUGen" Audio 2 [(11,0),(-1,3)] [Audio]
    , UGen "Sum3" Audio 0 [(12,0),(8,0),(3,0)] [Audio]
    , UGen "Rand" Scalar 0 [(-1,4),(-1,5)] [Scalar]
    , UGen "Rand" Scalar 0 [(-1,6),(-1,7)] [Scalar]
    , UGen "XLine" Control 0 [(14,0),(15,0),(-1,8),(-1,1)] [Control]
    , UGen "LPF" Audio 0 [(13,0),(16,0)] [Audio]
    , UGen "BinaryOpUGen" Audio 2 [(17,0),(4,0)] [Audio]
    , UGen "Pan2" Audio 0 [(18,0),(1,2),(1,1)] [Audio,Audio]
    , UGen "OffsetOut" Audio 0 [(0,0),(19,0),(19,1)] []
    ]
    []
  ]

playBuf :: SynthDef
playBuf = SynthDef
  [ GraphDef "PlayBuf_1"
    []
    [0.0,1.0,0.0,1.0,0.0,0.0,0.0]
    [("out",0),("bufnum",2),("rate",3),("trigger",1),("startPos",4),("loop",5)
    ,("doneAction",6)
    ]
    [ UGen "Control" Scalar 0 [] [Scalar]
    , UGen "TrigControl" Control 1 [] [Control]
    , UGen "Control" Control 2 [] [Control,Control,Control,Control,Control]
    , UGen "BufRateScale" Control 0 [(2,0)] [Control]
    , UGen "BinaryOpUGen" Control 2 [(3,0),(2,1)] [Control]
    , UGen "PlayBuf" Audio 0 [(2,0),(4,0),(1,0),(2,2),(2,3),(2,4)] [Audio]
    , UGen "OffsetOut" Audio 0 [(0,0),(5,0)] []
    ]
    []
  , GraphDef "PlayBuf_2"
    []
    [0.0,1.0,0.0,1.0,0.0,0.0,0.0]
    [("out",0),("bufnum",2),("rate",3),("trigger",1),("startPos",4),("loop",5)
    ,("doneAction",6)]
    [ UGen "Control" Scalar 0 [] [Scalar]
    , UGen "TrigControl" Control 1 [] [Control]
    , UGen "Control" Control 2 [] [Control,Control,Control,Control,Control]
    , UGen "BufRateScale" Control 0 [(2,0)] [Control]
    , UGen "BinaryOpUGen" Control 2 [(3,0),(2,1)] [Control]
    , UGen "PlayBuf" Audio 0 [(2,0),(4,0),(1,0),(2,2),(2,3),(2,4)] [Audio,Audio]
    , UGen "OffsetOut" Audio 0 [(0,0),(5,0),(5,1)] []
    ]
    []
  , GraphDef "PlayBuf_3"
    []
    [0.0,1.0,0.0,1.0,0.0,0.0,0.0]
    [("out",0),("bufnum",2),("rate",3),("trigger",1),("startPos",4),("loop",5)
    ,("doneAction",6)
    ]
    [ UGen "Control" Scalar 0 [] [Scalar]
    , UGen "TrigControl" Control 1 [] [Control]
    , UGen "Control" Control 2 [] [Control,Control,Control,Control,Control]
    , UGen "BufRateScale" Control 0 [(2,0)] [Control]
    , UGen "BinaryOpUGen" Control 2 [(3,0),(2,1)] [Control]
    , UGen "PlayBuf" Audio 0 [(2,0),(4,0),(1,0),(2,2),(2,3),(2,4)] [Audio,Audio,Audio]
    , UGen "OffsetOut" Audio 0 [(0,0),(5,0),(5,1),(5,2)] []
    ]
    []
  , GraphDef "PlayBuf_4"
    []
    [0.0,1.0,0.0,1.0,0.0,0.0,0.0]
    [("out",0),("bufnum",2),("rate",3),("trigger",1),("startPos",4),("loop",5)
    ,("doneAction",6)
    ]
    [ UGen "Control" Scalar 0 [] [Scalar]
    , UGen "TrigControl" Control 1 [] [Control]
    , UGen "Control" Control 2 [] [Control,Control,Control,Control,Control]
    , UGen "BufRateScale" Control 0 [(2,0)] [Control]
    , UGen "BinaryOpUGen" Control 2 [(3,0),(2,1)] [Control]
    , UGen "PlayBuf" Audio 0 [(2,0),(4,0),(1,0),(2,2),(2,3),(2,4)] [Audio,Audio,Audio,Audio]
    , UGen "OffsetOut" Audio 0 [(0,0),(5,0),(5,1),(5,2),(5,3)] []
    ]
    []
  , GraphDef "PlayBuf_5"
    []
    [0.0,1.0,0.0,1.0,0.0,0.0,0.0]
    [("out",0),("bufnum",2),("rate",3),("trigger",1),("startPos",4),("loop",5)
    ,("doneAction",6)
    ]
    [ UGen "Control" Scalar 0 [] [Scalar]
    , UGen "TrigControl" Control 1 [] [Control]
    , UGen "Control" Control 2 [] [Control,Control,Control,Control,Control]
    , UGen "BufRateScale" Control 0 [(2,0)] [Control]
    , UGen "BinaryOpUGen" Control 2 [(3,0),(2,1)] [Control]
    , UGen "PlayBuf" Audio 0 [(2,0),(4,0),(1,0),(2,2),(2,3),(2,4)] [Audio,Audio,Audio,Audio,Audio]
    , UGen "OffsetOut" Audio 0 [(0,0),(5,0),(5,1),(5,2),(5,3),(5,4)] []
    ]
    []
  , GraphDef "PlayBuf_6"
    []
    [0.0,1.0,0.0,1.0,0.0,0.0,0.0]
    [("out",0),("bufnum",2),("rate",3),("trigger",1),("startPos",4),("loop",5)
    ,("doneAction",6)
    ]
    [ UGen "Control" Scalar 0 [] [Scalar]
    , UGen "TrigControl" Control 1 [] [Control]
    , UGen "Control" Control 2 [] [Control,Control,Control,Control,Control]
    , UGen "BufRateScale" Control 0 [(2,0)] [Control]
    , UGen "BinaryOpUGen" Control 2 [(3,0),(2,1)] [Control]
    , UGen "PlayBuf" Audio 0 [(2,0),(4,0),(1,0),(2,2),(2,3),(2,4)] [Audio,Audio,Audio,Audio,Audio,Audio]
    , UGen "OffsetOut" Audio 0 [(0,0),(5,0),(5,1),(5,2),(5,3),(5,4),(5,5)] []
    ]
    []
  , GraphDef "PlayBuf_7"
    []
    [0.0,1.0,0.0,1.0,0.0,0.0,0.0]
    [("out",0),("bufnum",2),("rate",3),("trigger",1),("startPos",4),("loop",5)
    ,("doneAction",6)
    ]
    [ UGen "Control" Scalar 0 [] [Scalar]
    , UGen "TrigControl" Control 1 [] [Control]
    , UGen "Control" Control 2 [] [Control,Control,Control,Control,Control]
    , UGen "BufRateScale" Control 0 [(2,0)] [Control]
    , UGen "BinaryOpUGen" Control 2 [(3,0),(2,1)] [Control]
    , UGen "PlayBuf" Audio 0 [(2,0),(4,0),(1,0),(2,2),(2,3),(2,4)] [Audio,Audio,Audio,Audio,Audio,Audio,Audio]
    , UGen "OffsetOut" Audio 0 [(0,0),(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6)] []
    ]
    []
  , GraphDef "PlayBuf_8"
    []
    [0.0,1.0,0.0,1.0,0.0,0.0,0.0]
    [("out",0),("bufnum",2),("rate",3),("trigger",1),("startPos",4),("loop",5)
    ,("doneAction",6)
    ]
    [ UGen "Control" Scalar 0 [] [Scalar]
    , UGen "TrigControl" Control 1 [] [Control]
    , UGen "Control" Control 2 [] [Control,Control,Control,Control,Control]
    , UGen "BufRateScale" Control 0 [(2,0)] [Control]
    , UGen "BinaryOpUGen" Control 2 [(3,0),(2,1)] [Control]
    , UGen "PlayBuf" Audio 0 [(2,0),(4,0),(1,0),(2,2),(2,3),(2,4)] [Audio,Audio,Audio,Audio,Audio,Audio,Audio,Audio]
    , UGen "OffsetOut" Audio 0 [(0,0),(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7)] []
    ]
    []
  ]

