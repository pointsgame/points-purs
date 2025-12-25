module Countdown where

import Prelude

import CSS as CSS
import Data.Int as Int
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Elements as HEl
import Halogen.HTML.Properties as HP

countdown :: Boolean -> Milliseconds -> HH.PlainHTML
countdown active duration =
  if active then activeCountdown duration else idleCountdown duration

activeCountdown :: Milliseconds -> HH.PlainHTML
activeCountdown duration =
  let
    millisLeft = Int.round $ unwrap duration
    secsLeft = millisLeft / 1000
    minsLeft = secsLeft / 60
    secsRest = secsLeft `mod` 60
    millisRest = millisLeft `mod` 1000
    delay = Int.toNumber $ (secsRest - 60) * 1000 + millisRest
  in
    HH.div
      [ HCSS.style $ CSS.display CSS.flex
      ]
      [ HEl.style [ HP.type_ $ MediaType "text/css" ]
          [ HH.text
              " @property --minutes {    \
              \   syntax: \"<integer>\"; \
              \   inherits: false;       \
              \   initial-value: 0;      \
              \ }                        \
              \                          \
              \ @property --seconds {    \
              \   syntax: \"<integer>\"; \
              \   inherits: false;       \
              \   initial-value: 0;      \
              \ }                        "
          ]
      , HCSS.stylesheet do
          CSS.keyframesFromTo
            "minutes-count"
            (CSS.key (CSS.fromString "--minutes") (show $ minsLeft + 1))
            (CSS.key (CSS.fromString "--minutes") "0")
          CSS.keyframesFromTo
            "seconds-count"
            (CSS.key (CSS.fromString "--seconds") "60")
            (CSS.key (CSS.fromString "--seconds") "0")
          (CSS.star CSS.& CSS.byClass "minutes") CSS.& CSS.pseudo ":after" CSS.? do
            CSS.key (CSS.fromString "counter-reset") "number calc(mod(var(--minutes), 60))"
            CSS.animation
              (CSS.fromString "minutes-count")
              (CSS.ms (delay - 30000.0))
              CSS.linear
              (CSS.sec $ Int.toNumber $ (minsLeft + 1) * 60)
              (CSS.iterationCount 1.0)
              CSS.normalAnimationDirection
              CSS.forwards
            CSS.key (CSS.fromString "content") "counter(number, decimal-leading-zero)"
          (CSS.star CSS.& CSS.byClass "seconds") CSS.& CSS.pseudo ":after" CSS.? do
            CSS.key (CSS.fromString "counter-reset") "number calc(mod(var(--seconds), 60))"
            CSS.animation
              (CSS.fromString "seconds-count")
              (CSS.ms (delay - 500.0))
              CSS.linear
              (CSS.sec 60.0)
              (CSS.iterationCount $ Int.toNumber $ minsLeft + 1)
              CSS.normalAnimationDirection
              CSS.forwards
            CSS.key (CSS.fromString "content") "counter(number, decimal-leading-zero)"
      , HH.div [ HP.class_ $ wrap "minutes" ] []
      , HH.label_ [ HH.text ":" ]
      , HH.div [ HP.class_ $ wrap "seconds" ] []
      ]

idleCountdown :: Milliseconds -> HH.PlainHTML
idleCountdown duration =
  let
    millisLeft = Int.round $ unwrap duration
    secsLeft = millisLeft / 1000
    minsLeft = secsLeft / 60
    secsRest = secsLeft `mod` 60
  in
    HH.div
      [ HCSS.style $ CSS.display CSS.flex
      ]
      [ HH.label_ [ HH.text $ (if minsLeft < 10 then "0" else "") <> show minsLeft <> ":" <> (if secsRest < 10 then "0" else "") <> show secsRest ]
      ]
