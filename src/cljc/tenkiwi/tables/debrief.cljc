(ns tenkiwi.tables.debrief
  )

(def company-values [
                     "Ruthless Compassion"
                     "Methodical Efficiency"
                     "Explosive Calm"
                     "Paranoid Cleanliness"
                     "Extreme Moderation"
                     "Overconfident Humility"
                     "Gracious Cynicism"
                     "Obvious Covertness"
                     "Careful Hyperactivity"
                     "Joyful Seriousness"
                     "Aggressive Passivity"
                     "Disrespectful Agreement"
                     "Cautious Optimism"
                     ])

(def value-modifiers
  ["Ruthless"
   "Methodical"
   "Explosive"
   "Paranoid"
   "Delusional"
   "Overt"
   "Gracious"
   "Extreme"
   "Careful"
   "Joyful"
   "Respectful"
   "Cautious"])

(def value-bases
  ["Compassion"
   "Efficiency"
   "Cleanliness"
   "Moderation"
   "Humility"
   "Cynicism"
   "Guile"
   "Hyperactivity"
   "Seriousness"
   "Disagreement"
   "Optimism"])

(def agent-first-names
  ["Axel"
   "Alice"
   "Andrea"
   "Bonni"
   "Carl"
   "Carol"
   "David"
   "Dana"
   "Johann"
   "Jackie"
   "John"
   "Lauren"
   "Luke"
   "Steven"
   "Sean"
   "Sven"
   "Patricia"
   "Pam"
   "Paul"
   "Roger"
   "Julia"
   "James"])

(def agent-last-names
  ["Gutenberg"
   "Farmer"
   "Nixon"
   "Hill"
   "Edwards"
   "Waterford"
   "Johnson"
   "Moore"
   "Connery"
   "Haslem"
   "Scully"
   "Applegate"
   "Coolidge"
   "Rockefeller"
   "Smith"
   "Bond"
   ])

(def code-names
  ["Green"
   "Orange"
   "Gold"
   "Silver"
   "Blue"
   "Red"
   "Purple"
   "Magenta"
   "Fuchsia"
   "Mauve"
   "Rabbit"
   "Tinman"
   "Carrots"
   "lol"
   "Eggplant"
   "Facepalm"
   "Duchess"
   "003"
   "X"
   "42"
   "Crying Face"
   "Salsa"
   "Panda"
   "Tango"
   "Foxtrot"
   "Farquad"
   "Cricket"
   "Lion"])

(def team-skills
  ["Driver"
   "Pilot"
   "First Aid"
   "Doctor"
   "High-speed Pursuit"
   "Parkour"
   "Demolitions"
   "MS Excel"
   "Disguise"
   "Team Morale"
   "Thief"
   "Researcher"
   "Infilitration"
   "Extraction"
   "Forgery"
   "Heavy Weaponry"
   "Karate"
   "Close Combat"
   "Martial Arts"
   "Powerpoint"
   "Sniper"
   "Archery"
   "Hacking"
   "Therapy"
   "Fast Talker"
   "Hairdressing"
   "Intern"
   "Coop Student"
   "Artist"
   "Chef"
   "Janitorial Services"])

(defn random-name []
  (str (rand-nth agent-first-names) " " (rand-nth agent-last-names)))

(defn random-codename []
  (str "Agent " (rand-nth code-names)))

(defn random-skill []
  (rand-nth team-skills))

;; TODO: figure out a good way to make sure this isn't repetitive
(defn random-value []
  (str (rand-nth value-modifiers) " " (rand-nth value-bases)))
