type FirstName = String
type LastName = String
type PatientName = (FirstName, LastName)
type Age = Int
type Height = Int
type Weight = Int

patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fname, lname) age height = name ++ " " ++ ageHeight
    where name = fname ++ ", " ++ lname
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type MiddleName = String
type ShortMiddleName = String
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName | NameWithShortMiddleName FirstName ShortMiddleName LastName

showName :: Name -> String
showName (Name fn ln) = fn ++ " " ++ ln
showName (NameWithMiddle fn mn ln) = fn ++ " " ++ mn ++ " " ++ ln
showName (NameWithShortMiddleName fn mn ln) = fn ++ " " ++ mn ++ ". " ++ ln

data Patient = Patient { name::Name
                       , sex :: Sex
                       , age :: Age
                       , height :: Height
                       , weight :: Weight
                       , bt:: BloodType }

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeESmith :: Patient
janeESmith = Patient (NameWithShortMiddleName "Jane" "E" "Smith") Female 20 68 130 (BloodType O Neg)

jackieSmith :: Patient
jackieSmith = Patient { name = Name "Jackie" "Smith"
                      , sex = Female
                      , age = 15
                      , height = 68
                      , weight = 120
                      , bt = BloodType A Pos  
                      }

jackieSmithAfterBirthDay :: Patient
jackieSmithAfterBirthDay = jackieSmith { age = 16 }

canDonateToPatients :: Patient -> Patient -> Bool
canDonateToPatients patient1 patient2 = canDonateTo (bt patient1) (bt patient2)

showOfficialName :: Name -> String
showOfficialName (Name fn ln) = ln ++ ", " ++ fn
showOfficialName (NameWithMiddle fn mn ln) = ln ++ ", " ++ fn ++ " " ++ mn
showOfficialName (NameWithShortMiddleName fn mn ln) = ln ++ ", " ++ fn ++ " " ++ mn ++ "."

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

patientSummary :: Patient -> String
patientSummary patient = "**************" ++ "\nPatient Name: " ++ (showOfficialName (name patient)) ++ "\nSex: "
    ++ (showSex (sex patient)) ++ "\nAge: " ++ (show (age patient)) ++ "\nHeight: " ++ (show (height patient)) ++ "in." ++ "\nWeight: "
    ++ (show (weight patient)) ++ "lbs." ++ "\nBlood Type: " ++ (showBloodType (bt patient)) ++ "\n**************"