(ns app.views
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            ["highcharts" :as highcharts]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            ["uuid" :as uuid]
            [goog.string :as gstring :refer [format]]
            [goog.string.format]))

(def state (atom {:prv-price 0
                  :nodes 0
                  :pnodes 0
                  :restake? false
                  :time 6
                  :yearly-issuance (* 0.2 8751970)
                  :network-stake 0
                  :network-inc 1
                  :price-inc 0
                  :navbar-open false
                  :chart-open false
                  :stake 2000
                  :validator 1
                  :delegate 250
                  :loaded false}))
(def data (atom []))

(defn price-request []
  (go (let [response (<! (http/get "https://api.incognito.org/ptoken/list" {:with-credentials? false :headers {"Content-Type" "application/json"}}))
            last-trade (first (:Result (:body response)))
            price (/ (:PriceUsd last-trade) (:PricePrv last-trade))]
        (swap! state assoc :prv-price price))))

(defn nodes-request []
  (go (let [response (<! (http/post "https://mainnet.incognito.org/fullnode" {:json-params {:jsonrpc "1.0" :method "getbeaconbeststatedetail" :id (uuid/v4)} :with-credentials? false :headers {"Content-Type" "application/json"}}))
            nodes (count (:RewardReceiver (:Result (:body response))))]
        (do
          (swap! state assoc :nodes nodes)
          (swap! state assoc :network-stake (* 1750 nodes))
          (swap! state assoc :loaded true)))))

(defn holding-vector [issuance-vector network-stake holding]
  (let [network-inc (- 1 (/ (@state :network-inc) 100))
        pool-share (/ (@state :delegate) network-stake)
        pool-reward (* (@state :delegate) (/ 0.37 12))]
    (if (@state :restake?)
      (reduce #(conj %1 (int (+ (last %1) (+ pool-reward (* %2 (* network-inc (- (/ (last %1) network-stake) pool-share)))))))
              [holding]
              issuance-vector)
      (reduce #(conj %1 (int (+ (last %1) (+ pool-reward (* %2 (* network-inc (- (/ (first %1) network-stake) pool-share)))))))
              [holding]
              issuance-vector))))

(defn chart-component []
  (let [chartData {:xAxis {:categories []
                           :title {:text "Month"}}
                   :yAxis {:title {:text "Holding (PRV)"}}
                   :series [{:data @data}]
                   :tooltip {:pointFormat "<b>{point.y:.2f}</b> PRV <br>"
                             :headerFormat ""}
                   :plotOptions {:series {:color "black"}}
                   :legend {:enabled false}
                   :credits {:enabled false}
                   :title {:style {:display "none"}}}]
    (.chart highcharts "rev-chartjs" (clj->js chartData))))

(defn stake-chart []
  (reagent/create-class
   {:component-did-mount #(chart-component)
    :component-did-update #(chart-component)
    :display-name "chartjs-component"
    :reagent-render (fn []
                      @data
                      [:div#rev-chartjs])}))

(defn not-nan [value]
  (if (js/Number.isNaN value) 0 value))
(defn not-inf [value]
  (if-not (js/isFinite value) 0 value))
(defn vformat [value]
  (if (< 100 value) (format "%.0f" (not-nan value)) (format "%.2f" value)))
(defn pformat [percent]
  (format "%.2f" (* 100 (not-nan percent))))
(defn event-value [e]
  (not-nan (js/parseInt (-> e .-target .-value))))

(defn actual-issuance [past-years] (reduce #(* %1 0.91) (@state :yearly-issuance) (range past-years)))

(defn issuance-calc [time]
  (let [past-years (- (.getFullYear (js/Date.)) 2020)
        rem-months (- 12 (.getMonth (js/Date.)))
        years (int (/ (- time rem-months) 12))]
    (into []
          (for [i (range (not-nan time))]
            (if (< i rem-months)
              (/ (actual-issuance past-years) 12)
              (/ (actual-issuance (+ past-years (+ 1 (int (/ (- i rem-months) 12))))) 12))))))

(defn tooltip [& text]
  [:span.tooltip "i"
   [:span.tooltiptext text]])

(defn num-input [label value max class tooltiptext disabled]
  [:div
   [:label {:class class} label
    (when tooltiptext (tooltip tooltiptext))
    [:input {:type "number"
             :min 0
             :disabled disabled
             :value (not-nan (if max (min max (@state value)) (@state value)))
             :on-change #(swap! state assoc (keyword value) (not-nan (js/parseInt (min max (-> % .-target .-value)))))}]]])

(defn navbar []
  [:nav
   [:div.container
    [:div.navbar__brand
     [:a {:href "https://incognito.org/" :target "_blank"}
      [:img {:src "./images/logo.png" :width "25px"}]
      [:p "Incognito Calculator"]]]
    [:div.collapse {:class [(when (not (@state :navbar-open)) "u-hideOnMobile")]}
     [:a {:href "https://incognito.org/t/node/338"  :target "_blank"} "NODE"]
     [:a {:href "https://incognito.org/t/prv-holders/792"  :target "_blank"} "PRV"]
     [:p (format "%.3f" (@state :prv-price)) " USD"]]
    [:button.navbar__togglr {:on-click #(swap! state assoc :navbar-open (not (@state :navbar-open)))}
     [:span.navbar__togglr__bars]
     [:span.navbar__togglr__bars]
     [:span.navbar__togglr__bars]]]])

(defn dashboard []
  (let [prv-price (@state :prv-price)
        time (@state :time)
        stake (@state :stake)
        validator (@state :validator)
        delegate (@state :delegate)

        network-stake (- (@state :network-stake) (* 176 1750))
        holding (+ delegate (* validator 1750))
        network-share (max 0 (/ holding network-stake))

        holding-vector (holding-vector (issuance-calc time) network-stake holding) _ (reset! data holding-vector)

        pnode-reward (/ (* (@state :yearly-issuance) (/ (* 577 (@state :pnodes)) network-stake)) 12)

        future-prv-price (* prv-price (+ 1 (/ (@state :price-inc) 100)))

        m-inc (max 0 (+ (/ (- (last holding-vector) holding) time) pnode-reward))
        y-inc (* m-inc 12)
        d-inc (/ y-inc 365)

        y-rate (/ y-inc holding)

        reward-value (+ (* m-inc time) pnode-reward)
        reward-rate (/ reward-value holding)

        common-reward 32
        reward-freq (/ (/ 30 (/ (/ (/ (@state :yearly-issuance) (@state :nodes)) 12) common-reward)) validator)

        d-inc-usd (* future-prv-price d-inc)
        y-inc-usd (* future-prv-price y-inc)
        m-inc-usd (* future-prv-price m-inc)
        holding-usd (* prv-price holding)
        future-holding-usd (* future-prv-price holding)
        reward-value-usd (+ (* future-prv-price reward-value) (- future-holding-usd holding-usd))
        range-percent (* 100 (/ holding 10000))]
    [:main
     [:div.container
      [:div#settings.card
       [:h2.title "Staking settings"]
       [:form
        [:div>label.showUnit.showUnit--piece "Nodes (1750 PRV)"
         [:input {:type "number"
                  :min 0
                  :value (not-nan validator)
                  :on-change #(do (swap! state assoc :stake (+ delegate (* 1750 (event-value %))))
                                  (swap! state assoc :validator (event-value %)))}]]
        [:div>label.showUnit.showUnit--prv "Pool"
         [:input {:type "number"
                  :min 0
                  :value (not-nan delegate)
                  :on-change #(do (swap! state assoc :stake (+ (event-value %) (* 1750 validator)))
                                  (swap! state assoc :delegate (event-value %)))}]]
        [:div.u-fillLeft_fitRight
         [:label "Stake"
          (tooltip "Nodes + Pool")
          [:input {:style {:background (str "linear-gradient(to right, black 0%, black " range-percent "%, #fff " range-percent "%, white 100%)")}
                   :type "range"
                   :min 0
                   :max 10000
                   :value (not-nan holding)
                   :on-change #(do (swap! state assoc :stake (event-value %))
                                   (swap! state assoc :validator (int (/ (event-value %) 1750)))
                                   (swap! state assoc :delegate (rem (event-value %) 1750)))}]]
         [:div
          [:label.showUnit.showUnit--prv
           [:input {:type "number"
                    :min 0
                    :value (not-nan holding)
                    :on-change #(do (swap! state assoc :stake (event-value %))
                                    (swap! state assoc :validator (int (/ (event-value %) 1750)))
                                    (swap! state assoc :delegate (rem (event-value %) 1750)))}]]]]
        [num-input "Staking Time" :time 120 "showUnit showUnit--months" false]
        [:div>label.switch "Restake"
         (tooltip "Restake your income monthly")
         [:input#autorestake {:type "checkbox" :checked (@state :restake?) :on-click #(swap! state assoc :restake? (not (@state :restake?)))}]
         [:span.slider]]
        [:h3.title.title--secondary "Advanced"]
        [num-input "Price Increase" :price-inc nil "showUnit showUnit--percentage" "Applies to the whole term" false]
        [num-input "Monthly Network Increase" :network-inc 100 "showUnit showUnit--percentage" false]
        [num-input "Funded physical nodes" :pnodes nil "showUnit showUnit--piece" "35% of the whole reward" "disabled"]
        [num-input "Network Stake" :network-stake nil "showUnit showUnit--prv" "Staked PRV by active nodes" false]]
       [:p.disclaimer [:strong "Disclaimer: "] "This calculator only estimates. Node earnings rate is random and inconsistent."]]
      [:div#earnings_chart.card {:class [(when (@state :chart-open) "earnings_chart--showChart")]}
       [:h2.title "Earnings"]
       [:div#earnings_chart__chartWrapper
        (if (:loaded @state)
          [stake-chart]
          [:div#rev-chartjs.flex
           [:div.loader]])]
       [:div.dataBlock {:on-click #(swap! state assoc :chart-open (not (@state :chart-open)))}
        [:p "Daily Income"
         (tooltip "AVG of the whole term")]
        [:strong "$" (vformat d-inc-usd)]
        [:p (vformat d-inc) " PRV"]]
       [:div.dataBlock {:on-click #(swap! state assoc :chart-open (not (@state :chart-open)))}
        [:p "Monthly Income"
         (tooltip "AVG of the whole term")]
        [:strong "$" (vformat m-inc-usd)]
        [:p (vformat m-inc) " PRV"]]
       [:div.dataBlock {:on-click #(swap! state assoc :chart-open (not (@state :chart-open)))}
        [:p "Yearly Income"
         (tooltip "AVG of the whole term")]
        [:strong "$" (vformat y-inc-usd)]
        [:p (vformat y-inc) " PRV"]]]
      [:div#earnings_more.card
       [:div.dataBlock
        [:p "Total Reward Rate"]
        [:strong (pformat (not-inf reward-rate)) "%"]]
       [:div.dataBlock
        [:p "Yearly Reward Rate"]
        [:strong (pformat (not-inf y-rate)) "%"]]
       [:div.dataBlock
        [:p "Network Share"
         (tooltip "Your holding share" [:br] "in the network currently")]
        [:strong (format "%.4f" (* 100 network-share)) "%"]]
       [:div.dataBlock
        [:p "Current Holdings"]
        [:strong "$" (vformat holding-usd)]
        [:p (vformat holding) " PRV"]]
       [:div.dataBlock
        [:p "Total Rewards Value"]
        [:strong "$" (vformat reward-value-usd)]
        [:p (vformat reward-value) " PRV"]]
       [:div.dataBlock
        (if (zero? validator)
          [:div
           [:p "Reward Frequency"]
           [:strong "1 sec"]
           [:p "Pool brings constant reward"]]
          [:div
           [:p "Reward Frequency (Nodes)"]
           [:strong
            (cond
              (> 0.0416 reward-freq) (str (int (* 60 (* 24 reward-freq))) " minutes")
              (> 1 reward-freq) (str (format "%.1f" (* 24 reward-freq)) " hours")
              (< 1 reward-freq) (str (format "%.1f" reward-freq) " days"))]
           [:p (str "~" common-reward " PRV")]])]]
      [:div#about
       [:p "Made by " [:a {:href "https://zgen.hu" :target "_blank"} "ZGEN DAO"] " the bureaucracy-free online guild."]
       [:p "Send your feature requests to: " [:a {:href "mailto:contact@zgen.hu" :target "_blank"} "crypto@zgen.hu"]]
       [:p "Source: " [:a {:href "https://github.com/zgendao/incognito-stake-calculator" :target "_blank"} "zgendao/incognito-stake-calculator"]]]]]))

(defn app []
  (price-request)
  (nodes-request)
  [:<>
   [navbar]
   [dashboard]])
