library(dagitty)
dag {
  "Age difference" [exposure,pos="-1.495,0.164"]
  "Focal age" [exposure,pos="-2.081,-0.497"]
  "Partner age" [exposure,pos="-2.202,0.741"]
  "bull ID" [adjusted,pos="1.375,1.617"]
  "focal age relative to group" [pos="0.574,-0.359"]
  "group size" [adjusted,pos="-0.138,-1.574"]
  "is partner nearest neighbour" [pos="-1.233,1.210"]
  "look at speaker" [pos="-0.257,-0.493"]
  "look at vehicle" [pos="-1.316,-0.779"]
  "looking at nearest" [pos="-0.684,1.560"]
  "looking at partner" [outcome,pos="0.674,1.675"]
  "move to speaker" [pos="1.328,-0.443"]
  "move to vehicle" [pos="1.333,0.433"]
  "movement to partner" [pos="1.166,1.219"]
  "neighbour moving" [pos="-0.440,1.108"]
  "partner age relative to group" [pos="-1.851,1.622"]
  "perceived speaker distance" [pos="0.784,-0.944"]
  "playback ID" [adjusted,pos="0.810,-1.704"]
  "speaker distance" [adjusted,pos="1.166,-1.445"]
  "stimulus ID" [pos="0.375,-1.324"]
  "stimulus type" [pos="-0.091,-1.056"]
  "vehicle distance" [pos="-1.536,-1.579"]
  "Age difference" -> "looking at partner"
  "Age difference" -> "movement to partner"
  "Focal age" -> "Age difference"
  "Focal age" -> "focal age relative to group"
  "Focal age" -> "is partner nearest neighbour"
  "Focal age" -> "looking at partner"
  "Focal age" -> "movement to partner"
  "Focal age" -> "partner age relative to group"
  "Focal age" -> "vehicle distance"
  "Partner age" -> "Age difference"
  "Partner age" -> "focal age relative to group"
  "Partner age" -> "is partner nearest neighbour"
  "Partner age" -> "looking at partner"
  "Partner age" -> "movement to partner"
  "Partner age" -> "partner age relative to group"
  "bull ID" -> "looking at partner"
  "focal age relative to group" -> "is partner nearest neighbour"
  "focal age relative to group" -> "looking at partner"
  "focal age relative to group" -> "movement to partner"
  "group size" -> "focal age relative to group"
  "group size" -> "is partner nearest neighbour"
  "group size" -> "look at vehicle"
  "group size" -> "move to vehicle"
  "group size" -> "partner age relative to group"
  "group size" -> "vehicle distance"
  "is partner nearest neighbour" -> "looking at nearest"
  "is partner nearest neighbour" -> "looking at partner"
  "look at speaker" -> "looking at partner"
  "look at vehicle" -> "looking at partner"
  "looking at nearest" -> "looking at partner"
  "move to speaker" -> "movement to partner"
  "move to vehicle" -> "movement to partner"
  "movement to partner" -> "looking at partner"
  "movement to partner" -> "neighbour moving"
  "neighbour moving" -> "is partner nearest neighbour"
  "partner age relative to group" -> "is partner nearest neighbour"
  "partner age relative to group" -> "looking at partner"
  "partner age relative to group" -> "movement to partner"
  "perceived speaker distance" -> "looking at partner"
  "playback ID" -> "perceived speaker distance"
  "playback ID" -> "stimulus ID"
  "speaker distance" -> "look at speaker"
  "speaker distance" -> "looking at partner"
  "speaker distance" -> "move to speaker"
  "speaker distance" -> "movement to partner"
  "speaker distance" -> "perceived speaker distance"
  "stimulus ID" -> "look at speaker"
  "stimulus ID" -> "stimulus type"
  "stimulus type" -> "looking at partner"
  "stimulus type" -> "movement to partner"
  "vehicle distance" -> "look at vehicle"
  "vehicle distance" -> "looking at partner"
  "vehicle distance" -> "move to vehicle"
  "vehicle distance" -> "movement to partner"
}

The model implies the following conditional independences:
  
Age difference ⊥ bull ID
Age difference ⊥ focal age relative to group | Focal age, Partner age
Age difference ⊥ group size
Age difference ⊥ is partner nearest neighbour | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
Age difference ⊥ is partner nearest neighbour | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
Age difference ⊥ is partner nearest neighbour | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
Age difference ⊥ is partner nearest neighbour | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
Age difference ⊥ is partner nearest neighbour | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
Age difference ⊥ is partner nearest neighbour | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus ID, vehicle distance
Age difference ⊥ is partner nearest neighbour | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus type, vehicle distance
Age difference ⊥ is partner nearest neighbour | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus type, vehicle distance
Age difference ⊥ look at speaker
Age difference ⊥ look at vehicle | group size, vehicle distance
Age difference ⊥ look at vehicle | Focal age
Age difference ⊥ looking at nearest | is partner nearest neighbour
Age difference ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
Age difference ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
Age difference ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
Age difference ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
Age difference ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
Age difference ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus ID, vehicle distance
Age difference ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus type, vehicle distance
Age difference ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus type, vehicle distance
Age difference ⊥ move to speaker
Age difference ⊥ move to vehicle | group size, vehicle distance
Age difference ⊥ move to vehicle | Focal age
Age difference ⊥ neighbour moving | movement to partner
Age difference ⊥ partner age relative to group | Focal age, Partner age
Age difference ⊥ perceived speaker distance
Age difference ⊥ playback ID
Age difference ⊥ speaker distance
Age difference ⊥ stimulus ID
Age difference ⊥ stimulus type
Age difference ⊥ vehicle distance | Focal age
Focal age ⊥ Partner age
Focal age ⊥ bull ID
Focal age ⊥ group size
Focal age ⊥ look at speaker
Focal age ⊥ look at vehicle | group size, vehicle distance
Focal age ⊥ looking at nearest | is partner nearest neighbour
Focal age ⊥ move to speaker
Focal age ⊥ move to vehicle | group size, vehicle distance
Focal age ⊥ neighbour moving | movement to partner
Focal age ⊥ perceived speaker distance
Focal age ⊥ playback ID
Focal age ⊥ speaker distance
Focal age ⊥ stimulus ID
Focal age ⊥ stimulus type
Partner age ⊥ bull ID
Partner age ⊥ group size
Partner age ⊥ look at speaker
Partner age ⊥ look at vehicle
Partner age ⊥ looking at nearest | is partner nearest neighbour
Partner age ⊥ move to speaker
Partner age ⊥ move to vehicle
Partner age ⊥ neighbour moving | movement to partner
Partner age ⊥ perceived speaker distance
Partner age ⊥ playback ID
Partner age ⊥ speaker distance
Partner age ⊥ stimulus ID
Partner age ⊥ stimulus type
Partner age ⊥ vehicle distance
bull ID ⊥ focal age relative to group
bull ID ⊥ group size
bull ID ⊥ is partner nearest neighbour
bull ID ⊥ look at speaker
bull ID ⊥ look at vehicle
bull ID ⊥ looking at nearest
bull ID ⊥ move to speaker
bull ID ⊥ move to vehicle
bull ID ⊥ movement to partner
bull ID ⊥ neighbour moving
bull ID ⊥ partner age relative to group
bull ID ⊥ perceived speaker distance
bull ID ⊥ playback ID
bull ID ⊥ speaker distance
bull ID ⊥ stimulus ID
bull ID ⊥ stimulus type
bull ID ⊥ vehicle distance
focal age relative to group ⊥ look at speaker | stimulus ID
focal age relative to group ⊥ look at speaker | playback ID
focal age relative to group ⊥ look at speaker | group size
focal age relative to group ⊥ look at vehicle | group size, vehicle distance
focal age relative to group ⊥ look at vehicle | Focal age, group size
focal age relative to group ⊥ looking at nearest | is partner nearest neighbour
focal age relative to group ⊥ move to speaker
focal age relative to group ⊥ move to vehicle | group size, vehicle distance
focal age relative to group ⊥ move to vehicle | Focal age, group size
focal age relative to group ⊥ neighbour moving | movement to partner
focal age relative to group ⊥ partner age relative to group | Focal age, Partner age, group size
focal age relative to group ⊥ perceived speaker distance | playback ID
focal age relative to group ⊥ perceived speaker distance | group size
focal age relative to group ⊥ playback ID | group size
focal age relative to group ⊥ speaker distance
focal age relative to group ⊥ stimulus ID | playback ID
focal age relative to group ⊥ stimulus ID | group size
focal age relative to group ⊥ stimulus type | stimulus ID
focal age relative to group ⊥ stimulus type | playback ID
focal age relative to group ⊥ stimulus type | group size
focal age relative to group ⊥ vehicle distance | Focal age, group size
group size ⊥ look at speaker | stimulus ID
group size ⊥ look at speaker | playback ID
group size ⊥ looking at nearest | is partner nearest neighbour
group size ⊥ looking at partner | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
group size ⊥ looking at partner | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus ID, stimulus type, vehicle distance
group size ⊥ looking at partner | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, movement to partner, partner age relative to group, playback ID, speaker distance, stimulus type, vehicle distance
group size ⊥ looking at partner | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, move to vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
group size ⊥ looking at partner | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, move to vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus ID, vehicle distance
group size ⊥ looking at partner | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
group size ⊥ looking at partner | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, move to vehicle, neighbour moving, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
group size ⊥ looking at partner | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, move to vehicle, neighbour moving, partner age relative to group, perceived speaker distance, speaker distance, stimulus ID, vehicle distance
group size ⊥ looking at partner | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
group size ⊥ move to speaker
group size ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus type, vehicle distance
group size ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus ID, vehicle distance
group size ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, playback ID, vehicle distance
group size ⊥ neighbour moving | movement to partner
group size ⊥ neighbour moving | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus type, vehicle distance
group size ⊥ neighbour moving | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus ID, vehicle distance
group size ⊥ neighbour moving | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, playback ID, vehicle distance
group size ⊥ perceived speaker distance | playback ID
group size ⊥ speaker distance
group size ⊥ stimulus ID | playback ID
group size ⊥ stimulus type | stimulus ID
group size ⊥ stimulus type | playback ID
is partner nearest neighbour ⊥ look at speaker | speaker distance, stimulus ID
is partner nearest neighbour ⊥ look at speaker | playback ID, speaker distance, stimulus type
is partner nearest neighbour ⊥ look at speaker | group size, speaker distance, stimulus type
is partner nearest neighbour ⊥ look at speaker | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus ID, vehicle distance
is partner nearest neighbour ⊥ look at speaker | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ look at speaker | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
is partner nearest neighbour ⊥ look at speaker | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ look at speaker | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
is partner nearest neighbour ⊥ look at speaker | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
is partner nearest neighbour ⊥ look at vehicle | group size, vehicle distance
is partner nearest neighbour ⊥ look at vehicle | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
is partner nearest neighbour ⊥ look at vehicle | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
is partner nearest neighbour ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus type, vehicle distance
is partner nearest neighbour ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus ID, vehicle distance
is partner nearest neighbour ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus type, vehicle distance
is partner nearest neighbour ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
is partner nearest neighbour ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ move to speaker | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
is partner nearest neighbour ⊥ move to speaker | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
is partner nearest neighbour ⊥ move to vehicle | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
is partner nearest neighbour ⊥ move to vehicle | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
is partner nearest neighbour ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus type, vehicle distance
is partner nearest neighbour ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
is partner nearest neighbour ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ movement to partner | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
is partner nearest neighbour ⊥ perceived speaker distance | playback ID, speaker distance
is partner nearest neighbour ⊥ perceived speaker distance | group size, speaker distance, stimulus ID
is partner nearest neighbour ⊥ perceived speaker distance | group size, speaker distance, stimulus type
is partner nearest neighbour ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
is partner nearest neighbour ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
is partner nearest neighbour ⊥ playback ID | group size, stimulus ID
is partner nearest neighbour ⊥ playback ID | group size, stimulus type
is partner nearest neighbour ⊥ playback ID | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
is partner nearest neighbour ⊥ playback ID | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
is partner nearest neighbour ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus type, vehicle distance
is partner nearest neighbour ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus ID, vehicle distance
is partner nearest neighbour ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus type, vehicle distance
is partner nearest neighbour ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
is partner nearest neighbour ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ speaker distance | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
is partner nearest neighbour ⊥ speaker distance | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
is partner nearest neighbour ⊥ stimulus ID | playback ID, stimulus type
is partner nearest neighbour ⊥ stimulus ID | group size, stimulus type
is partner nearest neighbour ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
is partner nearest neighbour ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
is partner nearest neighbour ⊥ stimulus type | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus ID, vehicle distance
is partner nearest neighbour ⊥ stimulus type | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ stimulus type | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
is partner nearest neighbour ⊥ stimulus type | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
is partner nearest neighbour ⊥ stimulus type | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
is partner nearest neighbour ⊥ stimulus type | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
is partner nearest neighbour ⊥ vehicle distance | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
is partner nearest neighbour ⊥ vehicle distance | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
look at speaker ⊥ look at vehicle | group size
look at speaker ⊥ look at vehicle | playback ID
look at speaker ⊥ look at vehicle | stimulus ID
look at speaker ⊥ looking at nearest | is partner nearest neighbour
look at speaker ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
look at speaker ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
look at speaker ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
look at speaker ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
look at speaker ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
look at speaker ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus ID, vehicle distance
look at speaker ⊥ looking at nearest | group size, speaker distance, stimulus type
look at speaker ⊥ looking at nearest | playback ID, speaker distance, stimulus type
look at speaker ⊥ looking at nearest | speaker distance, stimulus ID
look at speaker ⊥ move to speaker | speaker distance
look at speaker ⊥ move to vehicle | group size
look at speaker ⊥ move to vehicle | playback ID
look at speaker ⊥ move to vehicle | stimulus ID
look at speaker ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, speaker distance, stimulus type, vehicle distance
look at speaker ⊥ movement to partner | group size, speaker distance, stimulus type
look at speaker ⊥ movement to partner | playback ID, speaker distance, stimulus type
look at speaker ⊥ movement to partner | speaker distance, stimulus ID
look at speaker ⊥ neighbour moving | movement to partner
look at speaker ⊥ neighbour moving | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, speaker distance, stimulus type, vehicle distance
look at speaker ⊥ neighbour moving | group size, speaker distance, stimulus type
look at speaker ⊥ neighbour moving | playback ID, speaker distance, stimulus type
look at speaker ⊥ neighbour moving | speaker distance, stimulus ID
look at speaker ⊥ partner age relative to group | group size
look at speaker ⊥ partner age relative to group | playback ID
look at speaker ⊥ partner age relative to group | stimulus ID
look at speaker ⊥ perceived speaker distance | playback ID, speaker distance
look at speaker ⊥ perceived speaker distance | speaker distance, stimulus ID
look at speaker ⊥ playback ID | stimulus ID
look at speaker ⊥ stimulus type | stimulus ID
look at speaker ⊥ vehicle distance | group size
look at speaker ⊥ vehicle distance | playback ID
look at speaker ⊥ vehicle distance | stimulus ID
look at vehicle ⊥ looking at nearest | is partner nearest neighbour
look at vehicle ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
look at vehicle ⊥ looking at nearest | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
look at vehicle ⊥ looking at nearest | group size, vehicle distance
look at vehicle ⊥ move to speaker
look at vehicle ⊥ move to vehicle | group size, vehicle distance
look at vehicle ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus type, vehicle distance
look at vehicle ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus ID, vehicle distance
look at vehicle ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, playback ID, vehicle distance
look at vehicle ⊥ movement to partner | group size, vehicle distance
look at vehicle ⊥ neighbour moving | movement to partner
look at vehicle ⊥ neighbour moving | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus type, vehicle distance
look at vehicle ⊥ neighbour moving | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus ID, vehicle distance
look at vehicle ⊥ neighbour moving | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, playback ID, vehicle distance
look at vehicle ⊥ neighbour moving | group size, vehicle distance
look at vehicle ⊥ partner age relative to group | Focal age, group size
look at vehicle ⊥ partner age relative to group | group size, vehicle distance
look at vehicle ⊥ perceived speaker distance | playback ID
look at vehicle ⊥ perceived speaker distance | group size
look at vehicle ⊥ playback ID | group size
look at vehicle ⊥ speaker distance
look at vehicle ⊥ stimulus ID | playback ID
look at vehicle ⊥ stimulus ID | group size
look at vehicle ⊥ stimulus type | stimulus ID
look at vehicle ⊥ stimulus type | playback ID
look at vehicle ⊥ stimulus type | group size
looking at nearest ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus type, vehicle distance
looking at nearest ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus ID, vehicle distance
looking at nearest ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
looking at nearest ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus type, vehicle distance
looking at nearest ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
looking at nearest ⊥ move to speaker | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
looking at nearest ⊥ move to speaker | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
looking at nearest ⊥ move to speaker | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
looking at nearest ⊥ move to speaker | is partner nearest neighbour
looking at nearest ⊥ move to vehicle | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
looking at nearest ⊥ move to vehicle | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
looking at nearest ⊥ move to vehicle | is partner nearest neighbour
looking at nearest ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus type, vehicle distance
looking at nearest ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
looking at nearest ⊥ movement to partner | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
looking at nearest ⊥ movement to partner | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
looking at nearest ⊥ movement to partner | is partner nearest neighbour
looking at nearest ⊥ neighbour moving | is partner nearest neighbour
looking at nearest ⊥ partner age relative to group | is partner nearest neighbour
looking at nearest ⊥ perceived speaker distance | playback ID, speaker distance
looking at nearest ⊥ perceived speaker distance | group size, speaker distance, stimulus ID
looking at nearest ⊥ perceived speaker distance | group size, speaker distance, stimulus type
looking at nearest ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
looking at nearest ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
looking at nearest ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
looking at nearest ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
looking at nearest ⊥ perceived speaker distance | is partner nearest neighbour
looking at nearest ⊥ playback ID | group size, stimulus ID
looking at nearest ⊥ playback ID | group size, stimulus type
looking at nearest ⊥ playback ID | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
looking at nearest ⊥ playback ID | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
looking at nearest ⊥ playback ID | is partner nearest neighbour
looking at nearest ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus type, vehicle distance
looking at nearest ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus ID, vehicle distance
looking at nearest ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
looking at nearest ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus type, vehicle distance
looking at nearest ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
looking at nearest ⊥ speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
looking at nearest ⊥ speaker distance | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
looking at nearest ⊥ speaker distance | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
looking at nearest ⊥ speaker distance | is partner nearest neighbour
looking at nearest ⊥ stimulus ID | playback ID, stimulus type
looking at nearest ⊥ stimulus ID | group size, stimulus type
looking at nearest ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
looking at nearest ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
looking at nearest ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
looking at nearest ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
looking at nearest ⊥ stimulus ID | is partner nearest neighbour
looking at nearest ⊥ stimulus type | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, stimulus ID, vehicle distance
looking at nearest ⊥ stimulus type | Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
looking at nearest ⊥ stimulus type | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, stimulus ID, vehicle distance
looking at nearest ⊥ stimulus type | Focal age, Partner age, focal age relative to group, move to vehicle, neighbour moving, partner age relative to group, playback ID, vehicle distance
looking at nearest ⊥ stimulus type | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
looking at nearest ⊥ stimulus type | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
looking at nearest ⊥ stimulus type | is partner nearest neighbour
looking at nearest ⊥ vehicle distance | Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group
looking at nearest ⊥ vehicle distance | Focal age, Partner age, focal age relative to group, group size, neighbour moving, partner age relative to group
looking at nearest ⊥ vehicle distance | is partner nearest neighbour
looking at partner ⊥ move to speaker | Age difference, Focal age, Partner age, focal age relative to group, move to vehicle, movement to partner, partner age relative to group, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ move to speaker | Age difference, Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ move to speaker | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, movement to partner, partner age relative to group, playback ID, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ move to speaker | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus ID, stimulus type, vehicle distance
looking at partner ⊥ move to speaker | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ move to vehicle | Age difference, Focal age, Partner age, focal age relative to group, group size, movement to partner, partner age relative to group, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ move to vehicle | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, movement to partner, partner age relative to group, playback ID, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ move to vehicle | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus ID, stimulus type, vehicle distance
looking at partner ⊥ move to vehicle | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ neighbour moving | Focal age, Partner age, focal age relative to group, group size, is partner nearest neighbour, movement to partner, partner age relative to group
looking at partner ⊥ neighbour moving | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, move to vehicle, movement to partner, partner age relative to group, playback ID, vehicle distance
looking at partner ⊥ neighbour moving | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, movement to partner, partner age relative to group, playback ID, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ neighbour moving | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, move to vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus ID, vehicle distance
looking at partner ⊥ neighbour moving | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, move to vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ neighbour moving | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus ID, stimulus type, vehicle distance
looking at partner ⊥ neighbour moving | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ playback ID | group size, perceived speaker distance, speaker distance, stimulus ID
looking at partner ⊥ playback ID | group size, look at speaker, perceived speaker distance, speaker distance, stimulus type
looking at partner ⊥ playback ID | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, move to vehicle, neighbour moving, partner age relative to group, perceived speaker distance, speaker distance, stimulus ID, vehicle distance
looking at partner ⊥ playback ID | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, move to vehicle, neighbour moving, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ playback ID | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, move to vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus ID, vehicle distance
looking at partner ⊥ playback ID | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, move to vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ playback ID | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus ID, stimulus type, vehicle distance
looking at partner ⊥ playback ID | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ stimulus ID | look at speaker, playback ID, speaker distance, stimulus type
looking at partner ⊥ stimulus ID | group size, look at speaker, perceived speaker distance, speaker distance, stimulus type
looking at partner ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, move to vehicle, neighbour moving, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, move to vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
looking at partner ⊥ stimulus ID | Age difference, Focal age, Partner age, focal age relative to group, is partner nearest neighbour, look at speaker, look at vehicle, movement to partner, partner age relative to group, perceived speaker distance, speaker distance, stimulus type, vehicle distance
move to speaker ⊥ move to vehicle
move to speaker ⊥ neighbour moving | movement to partner
move to speaker ⊥ partner age relative to group
move to speaker ⊥ perceived speaker distance | speaker distance
move to speaker ⊥ playback ID
move to speaker ⊥ stimulus ID
move to speaker ⊥ stimulus type
move to speaker ⊥ vehicle distance
move to vehicle ⊥ neighbour moving | movement to partner
move to vehicle ⊥ partner age relative to group | Focal age, group size
move to vehicle ⊥ partner age relative to group | group size, vehicle distance
move to vehicle ⊥ perceived speaker distance | playback ID
move to vehicle ⊥ perceived speaker distance | group size
move to vehicle ⊥ playback ID | group size
move to vehicle ⊥ speaker distance
move to vehicle ⊥ stimulus ID | playback ID
move to vehicle ⊥ stimulus ID | group size
move to vehicle ⊥ stimulus type | stimulus ID
move to vehicle ⊥ stimulus type | playback ID
move to vehicle ⊥ stimulus type | group size
movement to partner ⊥ perceived speaker distance | playback ID, speaker distance
movement to partner ⊥ perceived speaker distance | group size, speaker distance, stimulus ID
movement to partner ⊥ perceived speaker distance | group size, speaker distance, stimulus type
movement to partner ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, speaker distance, stimulus ID, vehicle distance
movement to partner ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, speaker distance, stimulus type, vehicle distance
movement to partner ⊥ playback ID | group size, stimulus ID
movement to partner ⊥ playback ID | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus ID, vehicle distance
movement to partner ⊥ playback ID | group size, stimulus type
movement to partner ⊥ playback ID | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus type, vehicle distance
movement to partner ⊥ stimulus ID | playback ID, stimulus type
movement to partner ⊥ stimulus ID | group size, stimulus type
movement to partner ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus type, vehicle distance
neighbour moving ⊥ partner age relative to group | movement to partner
neighbour moving ⊥ perceived speaker distance | playback ID, speaker distance
neighbour moving ⊥ perceived speaker distance | group size, speaker distance, stimulus ID
neighbour moving ⊥ perceived speaker distance | group size, speaker distance, stimulus type
neighbour moving ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, speaker distance, stimulus ID, vehicle distance
neighbour moving ⊥ perceived speaker distance | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, speaker distance, stimulus type, vehicle distance
neighbour moving ⊥ perceived speaker distance | movement to partner
neighbour moving ⊥ playback ID | group size, stimulus ID
neighbour moving ⊥ playback ID | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus ID, vehicle distance
neighbour moving ⊥ playback ID | group size, stimulus type
neighbour moving ⊥ playback ID | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus type, vehicle distance
neighbour moving ⊥ playback ID | movement to partner
neighbour moving ⊥ speaker distance | movement to partner
neighbour moving ⊥ stimulus ID | playback ID, stimulus type
neighbour moving ⊥ stimulus ID | group size, stimulus type
neighbour moving ⊥ stimulus ID | Focal age, Partner age, focal age relative to group, move to vehicle, partner age relative to group, stimulus type, vehicle distance
neighbour moving ⊥ stimulus ID | movement to partner
#neighbour moving ⊥ stimulus type | movement to partner
#neighbour moving ⊥ vehicle distance | movement to partner
#partner age relative to group ⊥ perceived speaker distance | playback ID
#partner age relative to group ⊥ perceived speaker distance | group size
#partner age relative to group ⊥ playback ID | group size
#partner age relative to group ⊥ speaker distance
#partner age relative to group ⊥ stimulus ID | playback ID
#partner age relative to group ⊥ stimulus ID | group size
#partner age relative to group ⊥ stimulus type | stimulus ID
#partner age relative to group ⊥ stimulus type | playback ID
#partner age relative to group ⊥ stimulus type | group size
#partner age relative to group ⊥ vehicle distance | Focal age, group size
#perceived speaker distance ⊥ stimulus ID | playback ID
#perceived speaker distance ⊥ stimulus type | stimulus ID
#perceived speaker distance ⊥ stimulus type | playback ID
#perceived speaker distance ⊥ vehicle distance | group size
#perceived speaker distance ⊥ vehicle distance | playback ID
#playback ID ⊥ speaker distance
#playback ID ⊥ stimulus type | stimulus ID
#playback ID ⊥ vehicle distance | group size
#speaker distance ⊥ stimulus ID
#speaker distance ⊥ stimulus type
#speaker distance ⊥ vehicle distance
#stimulus ID ⊥ vehicle distance | group size
#stimulus ID ⊥ vehicle distance | playback ID
#stimulus type ⊥ vehicle distance | group size
#stimulus type ⊥ vehicle distance | playback ID
#stimulus type ⊥ vehicle distance | stimulus ID