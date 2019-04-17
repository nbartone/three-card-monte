
#Aces, 9's, and 7's
Red_Deck =c(rep(14,4),
            rep(9,4),
            rep(7,4))

#Kings, Jacks, 6's
Blue_Deck =c(rep(13,4),
            rep(11,4),
            rep(6,4))

#Queens, Tens, 8's
Black_Deck =c(rep(12,4),
             rep(10,4),
             rep(8,4))

WinProb = function(deck1,
                   deck2,
                   sims){
Wins = 0
for(i in 1:sims){
draw_order1 = sample(deck1,
                    12,
                    replace = F)

draw_order2 = sample(deck2,
                     12,
                     replace = F)

a=0
b=0
c=1
while(a<5&b<5){
  
  if(draw_order1[c]>draw_order2[c]){
    a=a+1
  } else{
    b=b+1
  }
  c=c+1
}

if(a==5){
  Wins = Wins +1
}
}
  Wins / sims
}

WinProb(Red_Deck,Blue_Deck,100000)
WinProb(Red_Deck,Black_Deck,100000)
WinProb(Black_Deck,Blue_Deck,100000)


