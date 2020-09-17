type 'a t = 'a

external stage : 'a -> 'a t = "%identity"

external unstage : 'a t -> 'a = "%identity"
