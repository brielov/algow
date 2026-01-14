-- Project: Finite State Machine DSL
-- A complete FSM implementation with state transitions and execution

-- Core FSM types
type State a = State a

type Transition a b = Transition a b a

type Machine a b = Machine a (List (Transition a b))

-- State machine operations
let getState = s -> match s when State x -> x end

let makeTransition = from event to -> Transition from event to

let makeMachine = initial transitions -> Machine initial transitions

-- Find a transition for given state and event
let rec findTransition = state event transitions -> match transitions
  when Nil -> Nothing
  when Cons (Transition from ev to) rest ->
    if from == state && ev == event
    then Just to
    else findTransition state event rest
end

-- Step the machine with an event
let step = event machine state ->
  match machine when Machine _ transitions ->
    match findTransition (getState state) event transitions
      when Nothing -> state
      when Just newState -> State newState
    end
  end

-- Run multiple events
let rec run = events machine state -> match events
  when Nil -> state
  when Cons e rest -> run rest machine (step e machine state)
end

-- Check if in a specific state
let isInState = expected actual -> getState actual == expected

-- Example 1: Traffic Light FSM
type TrafficColor = Red | Yellow | Green

type TrafficEvent = Timer

let trafficMachine = makeMachine Red [ makeTransition Red Timer Green, makeTransition Green Timer Yellow, makeTransition Yellow Timer Red ]

-- Example 2: Turnstile FSM
type TurnstileState = Locked | Unlocked

type TurnstileEvent = Coin | Push

let turnstileMachine = makeMachine Locked [ makeTransition Locked Coin Unlocked, makeTransition Unlocked Coin Unlocked, makeTransition Unlocked Push Locked, makeTransition Locked Push Locked ]

-- Example 3: Vending Machine FSM
type VendingState = Idle | HasQuarter | Dispensing

type VendingEvent = InsertQuarter | EjectQuarter | TurnCrank | Dispense

let vendingMachine = makeMachine Idle [ makeTransition Idle InsertQuarter HasQuarter, makeTransition HasQuarter EjectQuarter Idle, makeTransition HasQuarter TurnCrank Dispensing, makeTransition Dispensing Dispense Idle ]

-- Example 4: Door FSM
type DoorState = Open | Closed | DoorLocked

type DoorEvent = OpenDoor | CloseDoor | Lock | Unlock

let doorMachine = makeMachine Closed [ makeTransition Closed OpenDoor Open, makeTransition Open CloseDoor Closed, makeTransition Closed Lock DoorLocked, makeTransition DoorLocked Unlock Closed ]

-- Example 5: Simple Game Character States
type CharacterState = Standing | Walking | Jumping | Falling

type CharacterEvent = Walk | Stop | Jump | Land | Fall

let characterMachine = makeMachine Standing [ makeTransition Standing Walk Walking, makeTransition Walking Stop Standing, makeTransition Standing Jump Jumping, makeTransition Walking Jump Jumping, makeTransition Jumping Fall Falling, makeTransition Falling Land Standing, makeTransition Jumping Land Standing ]

-- Helper to count transitions
let countTransitions = machine -> match machine when Machine _ ts -> List.length ts end

-- Helper to get all states reachable from initial
let rec collectStates = acc transitions -> match transitions
  when Nil -> List.nub acc
  when Cons (Transition from _ to) rest ->
    collectStates (from :: to :: acc) rest
end

let getStates = machine -> match machine when Machine init ts ->
  collectStates [init] ts
end

-- Tests
let tests =
  -- Traffic light tests
  [ Test.ok "traffic initial red" (isInState Red (State Red))
  , Test.ok "traffic red->green" (isInState Green (step Timer trafficMachine (State Red)))
  , Test.ok "traffic green->yellow" (isInState Yellow (step Timer trafficMachine (State Green)))
  , Test.ok "traffic yellow->red" (isInState Red (step Timer trafficMachine (State Yellow)))
  , Test.ok "traffic full cycle" (isInState Red (run [Timer, Timer, Timer] trafficMachine (State Red)))
  , Test.ok "traffic two cycles" (isInState Red (run [Timer, Timer, Timer, Timer, Timer, Timer] trafficMachine (State Red)))

    -- Turnstile tests
  , Test.ok "turnstile initial locked" (isInState Locked (State Locked))
  , Test.ok "turnstile coin unlocks" (isInState Unlocked (step Coin turnstileMachine (State Locked)))
  , Test.ok "turnstile push locks" (isInState Locked (step Push turnstileMachine (State Unlocked)))
  , Test.ok "turnstile push when locked stays" (isInState Locked (step Push turnstileMachine (State Locked)))
  , Test.ok "turnstile coin when unlocked stays" (isInState Unlocked (step Coin turnstileMachine (State Unlocked)))
  , Test.ok "turnstile sequence" (isInState Locked (run [Coin, Push] turnstileMachine (State Locked)))

    -- Vending machine tests
  , Test.ok "vending initial idle" (isInState Idle (State Idle))
  , Test.ok "vending insert quarter" (isInState HasQuarter (step InsertQuarter vendingMachine (State Idle)))
  , Test.ok "vending eject quarter" (isInState Idle (step EjectQuarter vendingMachine (State HasQuarter)))
  , Test.ok "vending turn crank" (isInState Dispensing (step TurnCrank vendingMachine (State HasQuarter)))
  , Test.ok "vending dispense" (isInState Idle (step Dispense vendingMachine (State Dispensing)))
  , Test.ok "vending full purchase" (isInState Idle (run [InsertQuarter, TurnCrank, Dispense] vendingMachine (State Idle)))

    -- Door tests
  , Test.ok "door initial closed" (isInState Closed (State Closed))
  , Test.ok "door open" (isInState Open (step OpenDoor doorMachine (State Closed)))
  , Test.ok "door close" (isInState Closed (step CloseDoor doorMachine (State Open)))
  , Test.ok "door lock" (isInState DoorLocked (step Lock doorMachine (State Closed)))
  , Test.ok "door unlock" (isInState Closed (step Unlock doorMachine (State DoorLocked)))
  , Test.ok "door cant open when locked" (isInState DoorLocked (step OpenDoor doorMachine (State DoorLocked)))

    -- Character tests
  , Test.ok "char initial standing" (isInState Standing (State Standing))
  , Test.ok "char walk" (isInState Walking (step Walk characterMachine (State Standing)))
  , Test.ok "char stop" (isInState Standing (step Stop characterMachine (State Walking)))
  , Test.ok "char jump from standing" (isInState Jumping (step Jump characterMachine (State Standing)))
  , Test.ok "char jump from walking" (isInState Jumping (step Jump characterMachine (State Walking)))
  , Test.ok "char fall" (isInState Falling (step Fall characterMachine (State Jumping)))
  , Test.ok "char land from falling" (isInState Standing (step Land characterMachine (State Falling)))
  , Test.ok "char sequence" (isInState Standing (run [Walk, Jump, Fall, Land] characterMachine (State Standing)))

    -- Machine inspection
  , Test.equal "traffic transitions" 3 (countTransitions trafficMachine)
  , Test.equal "turnstile transitions" 4 (countTransitions turnstileMachine)
  , Test.equal "vending transitions" 4 (countTransitions vendingMachine)
  , Test.equal "door transitions" 4 (countTransitions doorMachine)
  , Test.equal "character transitions" 7 (countTransitions characterMachine)

    -- Get states
  , Test.equal "traffic states count" 3 (List.length (getStates trafficMachine))
  , Test.equal "turnstile states count" 2 (List.length (getStates turnstileMachine))
  , Test.equal "door states count" 3 (List.length (getStates doorMachine))

    -- Door when already open (no transition defined for this)
  , Test.ok "door open when open stays" (isInState Open (step OpenDoor doorMachine (State Open)))
  ]

let main = args -> Test.run tests
