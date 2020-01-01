module Objectoriented where

-- ассоциативные списки
hosts​​ :: [([​Char​], [​Char​])]
hosts =
      [ (​"yandex.ru"​, ​"213.180.204.11"​)
      , (​"google.ru"​, ​"173.194.32.183"​)
      , (​"mail.ru"​, ​"94.100.180.199"​)
      , (​"linux.org.ru"​, ​"217.76.32.61"​)
      , (​"habrahabr.ru"​, ​"212.24.43.44"​)
      ]

data​ ​Transport​ = ​TCP​ | ​UDP

checkProtocol​ t = ​case​ t ​of
  ​TCP​ -> ​"That's TCP protocol"
  ​UDP​ -> ​"That's UDP protocol"

-- enum
data​ ​Color​ = ​Red​
           | ​Orange​
           | ​Yellow​
           | ​Green​
           | ​Blue​
           | ​Indigo​
           | ​Violet​ ​
           ​deriving​ (​Show​, ​Read​, ​Eq​, ​Enum​, ​Ord​)

-- АТД (алгебраич тип данных, составлен из более простых типов)
data​ ​IPAddress​ = ​IPv4​ ​String
               | ​IPv4Localhost
               | ​IPv6​ ​String

checkIP​ :: ​IPAddress​ -> ​String
checkIP​ addr = ​case​ addr ​of
  ​IPv4​ address -> ​"IPv4 is '"​ ++ address ++ ​"'."
  ​IPv4Localhost​ -> ​"IPv4, localhost."
  ​IPv6​ address -> ​"IPv6 is '"​ ++ address ++ ​"'."


-- АТД с именованным полями
data​ ​Person​ = ​Person ​ { ​firstName​ :: String
                      , ​lastName​  :: String
                      , ​email​     :: String
                      , ​age​       :: Int
                      }

-- Рекурсивный АТД
data​ ​Tree​ a = ​EmptyTree
            | ​Node​ a (​Tree​ a) (​Tree​ a) ​
            deriving​ (​Show​, ​Read​, ​Eq​)





main = do
  let t1 = zip [​1​,​2​,​3​] [​"one"​,​"two"​,​"three"​]
  print t1
  -- [(​1​,​"one"​),(​2​,​"two"​),(​3​,​"three"​)]

  let t2 = unzip [(​1​, ​"one"​), (​2​, ​"two"​), (​3​, ​"three"​)]
  print t2
  -- ([​1​,​2​,​3​],[​"one"​,​"two"​,​"three"​])

  let t3 = lookup ​1​ [(​1​,​"one"​),(​2​,​"two"​),(​3​,​"three"​)]
  print t3
  -- Just​ ​"one"

  let t4 = lookup ​0​ [(​1​,​"one"​),(​2​,​"two"​),(​3​,​"three"​)]
  print t4
  -- Nothing

  -- АТД
  let​ ip = ​IPAddress​ ​"127.0.0.1"

  let john = Person { firstName = "John"
                    , lastName = "Malkovich"
                    , email = "john.m@gmail.com"
                    , age = 24
                    }

  let​ name = firstName john
  -- "john"

  let​ anthony = john { firstName = ​"anthony"​ }
  -- same but: name == anthony

