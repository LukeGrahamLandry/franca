Object subclass: #Boolean !
Boolean subclass: #True !
Boolean subclass: #False !

! Boolean methods !
ifFalse: aBlock
    ^self ifTrue: [nil] ifFalse: aBlock !
ifTrue: aBlock
    ^self ifTrue: aBlock ifFalse: [nil] ! !

! True methods !
ifTrue: aBlock ifFalse: _
    ^aBlock value ! !

! False methods !
ifTrue: _ ifFalse: aBlock
    ^aBlock value ! !
