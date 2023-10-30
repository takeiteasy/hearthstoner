#!/usr/bin/env hy
(import hearthstone [cardxml])
(import hearthstone.enums *)
(import xml.etree [ElementTree])
(import sqlite3)
(import os)

(setv **card-strings** [GameTag.CARDNAME
                        GameTag.CARDTEXT
                        GameTag.FLAVORTEXT
                        GameTag.ARTISTNAME
                        GameTag.TARGETING_ARROW_TEXT
                        GameTag.HOW_TO_EARN_GOLDEN
                        GameTag.HOW_TO_EARN])
(setv **card-values** [GameTag.HEALTH
                       GameTag.CARD_SET
                       GameTag.CLASS
                       GameTag.FACTION
                       GameTag.CARDTYPE
                       GameTag.RARITY
                       GameTag.COST
                       GameTag.SPELL_SCHOOL
                       GameTag.ATK
                       GameTag.DURABILITY
                       GameTag.CARDRACE])
(setv **playable-cards** [CardType.HERO
                          CardType.MINION
                          CardType.SPELL
                          CardType.WEAPON
                          CardType.LOCATION])
(setv **card-mechanics** [])

(defmacro exec-sql-file [path]
  `(.execute db (.read (open (+ "tools/" ~path) "r"))))

(defmacro exec-sql-files [#* paths]
  `(for [p [~@paths]]
     (exec-sql-file p)))

(when (os.path.exists "cards.db")
  (os.remove "cards.db"))

(defmacro chain [#* arrays]
  `(let [result []]
    (for [a [~@arrays]]
      (setv result (+ result a)))
    result))

(defn sanitize [string]
  (.replace (.replace string "\"" "\"\"") "'" "''"))

(defmacro process-card [card-strings card-tags]
  `(let [strings (lfor v **card-strings** v.name)
         string-values (dfor key **card-strings**
                             :setv value (get ~card-strings key)
                             key.name
                             (cond
                               (not (len value)) None 
                               (isinstance value str) (sanitize value)
                               True (sanitize (get value "enUS"))))
         values (lfor v **card-values** v.name)
         card-values (dfor key **card-values**
                           key.name
                           (if (in key ~card-tags)
                             (get ~card-tags key)
                             None))]
     (| string-values card-values)))

(let [db (sqlite3.connect "cards.db")]
  (exec-sql-files "cards.sql" "mechanics.sql" "mapping.sql")
  (let [fh (open "hsdata/CardDefs.xml" "rb")
        context (ElementTree.iterparse fh :events ["end"])]
    (for [[action elem] context]
      (when (and (= action "end")
                 (= elem.tag "Entity"))
        (let [card (cardxml.CardXML.from-xml elem)]
          (setv card.locale "enUS")
          (let [data (process-card card.strings card.tags)]
            (setv
              (get data "ID") (getattr card "dbf_id")
              (get data "CARDID") (getattr card "id")
              (get data "HERO_POWER") card.hero-power)
            (let [keys (lfor key (data.keys) f"\"{key}\"")
                  values (lfor value (data.values)
                              (if (not value)
                                "NULL"
                                (if (isinstance value str)
                                  f"\"{value}\""
                                  (str value))))]
              (.execute db f"INSERT INTO \"CARDS\" ({(.join "," keys)}) VALUES ({(.join "," values)});")))))))
  (.commit db)
  (.close db))