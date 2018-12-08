module Program = 
  open Tests  
  open Hedgehog
  let [<EntryPoint>] main _ = 

    //We run our properties here...
    Property.print Tests.propReverse
    0
