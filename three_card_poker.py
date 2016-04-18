import random

from collections import namedtuple
from itertools import combinations


def ranks_by_count(hand_info, count):
    return [card for card, qty in hand_info['rank_counts'].items() if qty == count]


def high_card_tie_breaker(hand_info_1, hand_info_2):
    sorted_1 = sorted(ranks_by_count(hand_info_1, 1), reverse=True)
    sorted_2 = sorted(ranks_by_count(hand_info_2, 1), reverse=True)
    for idx, hand_1_card in enumerate(sorted_1):
        if hand_1_card == sorted_2[idx]:
            continue
        return hand_info_1 if hand_1_card > sorted_2[idx] else hand_info_2
    return None


def x_of_a_kind_tie_breaker(hand_info_1, hand_info_2, x):
    h1_quad_rank = max(ranks_by_count(hand_info_1, x))
    h2_quad_rank = max(ranks_by_count(hand_info_2, x))

    if h1_quad_rank == h2_quad_rank:
        return high_card_tie_breaker(hand_info_1, hand_info_2)
    return hand_info_1 if h1_quad_rank > h2_quad_rank else hand_info_2


def four_of_a_kind_tie_breaker(hand_info_1, hand_info_2):
    return x_of_a_kind_tie_breaker(hand_info_1, hand_info_2, 4)


def three_of_a_kind_tie_breaker(hand_info_1, hand_info_2):
    return x_of_a_kind_tie_breaker(hand_info_1, hand_info_2, 3)


def pair_tie_breaker(hand_info_1, hand_info_2):
    return x_of_a_kind_tie_breaker(hand_info_1, hand_info_2, 2)


def two_pair_tie_breaker(hand_info_1, hand_info_2):
    h1_quad_rank = sorted(ranks_by_count(hand_info_1, 2), reverse=True)[:2]
    h2_quad_rank = sorted(ranks_by_count(hand_info_2, 2), reverse=True)[:2]

    if h1_quad_rank[0] == h2_quad_rank[0]:
        if h1_quad_rank[1] == h2_quad_rank[1]:
            return high_card_tie_breaker(hand_info_1, hand_info_2)
        return hand_info_1 if h1_quad_rank[1] > h2_quad_rank[1] else hand_info_2
    return hand_info_1 if h1_quad_rank[0] > h2_quad_rank[0]else hand_info_2


def full_house_tie_breaker(hand_info_1, hand_info_2):
    h1_trips_rank = max(ranks_by_count(hand_info_1, 3), reverse=True)
    h1_pair_rank = max([card for card in ranks_by_count(hand_info_1, 2) if card != h1_trips_rank])

    h2_trips_rank = max(ranks_by_count(hand_info_2, 3), reverse=True)
    h2_pair_rank = max([card for card in ranks_by_count(hand_info_2, 2) if card != h2_trips_rank])

    if h1_trips_rank == h2_trips_rank:
        if h1_pair_rank == h2_pair_rank:
            return high_card_tie_breaker(hand_info_1, hand_info_2)
        return hand_info_1 if h1_pair_rank > h2_pair_rank else hand_info_2
    return hand_info_1 if h1_trips_rank > h2_trips_rank else hand_info_2


Hand = namedtuple('Hand', ['name', 'test', 'tie_breaker'])

ROYAL_FLUSH = Hand(name='Royal Flush', test=lambda info: info['is_royal_flush'], tie_breaker=high_card_tie_breaker)
STRAIGHT_FLUSH = Hand(name='Straight Flush', test=lambda info: info['is_flush'] and info['is_straight'], tie_breaker=high_card_tie_breaker)
FOUR_OF_A_KIND = Hand(name='Four of a Kind', test=lambda info: info['is_four_of_a_kind'], tie_breaker=four_of_a_kind_tie_breaker)
FULL_HOUSE = Hand(name='Full House', test=lambda info: info['is_full_house'], tie_breaker=full_house_tie_breaker)
FLUSH = Hand(name='Flush', test=lambda info: info['is_flush'], tie_breaker=high_card_tie_breaker)
STRAIGHT = Hand(name='Straight', test=lambda info: info['is_straight'], tie_breaker=high_card_tie_breaker)
THREE_OF_A_KIND = Hand(name='Three of a Kind', test=lambda info: info['is_three_of_a_kind'],  tie_breaker=three_of_a_kind_tie_breaker)
TWO_PAIR = Hand(name='Two Pair', test=lambda info: info['is_two_pair'], tie_breaker=two_pair_tie_breaker)
PAIR = Hand(name='Pair', test=lambda info: info['is_pair'], tie_breaker=pair_tie_breaker)
HIGH_CARD = Hand(name='High Card', test=lambda info: True, tie_breaker=high_card_tie_breaker)


def hand_info(hand):
    ranks = sorted([card % 13 for card in hand])
    suits = [card / 13 for card in hand]
    is_straight = ranks in ([ranks[0] + x for x in range(len(hand))], [x for x in range(len(hand)-1)] + [ACE])

    is_flush = not [s for s in suits if s != suits[0]]
    is_pair = is_two_pair = is_three_of_a_kind = is_full_house = is_four_of_a_kind = is_straight_flush = is_royal_flush = False

    # A straight and flush ending in ace is a royal
    if is_flush and is_straight:
        is_straight_flush = True
        if ranks[-2:] == [ACE-1, ACE]:
            is_royal_flush = True

    rank_counts = {}
    for card in ranks:
        rank_counts[card] = rank_counts.setdefault(card, 0) + 1
    # If it's a straight, it cannot be boat/pair/2pr/3kind
    if not is_straight:
        pairs = [card for card, count in rank_counts.items() if count >= 2]
        is_pair = bool(pairs)
        is_two_pair = len(pairs) > 1

        triples = [card for card, count in rank_counts.items() if count >= 3]
        is_three_of_a_kind = bool(triples)

        is_full_house = bool(bool(set(pairs) - set(triples)) and triples)
        is_four_of_a_kind = [card for card, count in rank_counts.items() if count >= 4]
    return locals()


class ThreeCardPokerHand(object):
    VALUE_ORDER = (HIGH_CARD, PAIR, FLUSH, STRAIGHT, THREE_OF_A_KIND, STRAIGHT_FLUSH, ROYAL_FLUSH)

    def __init__(self, deck):
        self.hand = random.sample(deck, 3)
        self.hand_info = hand_info(self.hand)
        self.value = self.value_three_card_hand()

    def __cmp__(self, other):
        if self.value == other.value:
            winning_hand_info = self.value.tie_breaker(self.hand_info, other.hand_info)
            if winning_hand_info:
                return -1 if winning_hand_info == other.hand_info else 1
            return 0
        return 1 if self.VALUE_ORDER.index(self.value) > self.VALUE_ORDER.index(other.value) else -1

    def value_three_card_hand(self):
        for value in reversed(self.VALUE_ORDER):
            if value.test(self.hand_info):
                return value


ThreeCardPokerBet = namedtuple('ThreeCardPokerBet', ['ante', 'pairplus', 'sixcard'])

FIVE_CARD_VALUE_ORDER = (ROYAL_FLUSH, STRAIGHT_FLUSH, FOUR_OF_A_KIND, FULL_HOUSE, FLUSH, STRAIGHT, THREE_OF_A_KIND)
BET_RESULT_FOLD, BET_RESULT_DQ,  BET_RESULT_LOSE,  BET_RESULT_TIE,  BET_RESULT_WIN = range(5)
TRACKED_RESULTS = {
    BET_RESULT_DQ: 'DQ',  BET_RESULT_LOSE: 'Lose',  BET_RESULT_TIE:'Tie',  BET_RESULT_WIN: 'Win'}


def play_three_card(banker_hand, remaining_deck, player, banking=True):
    player_hand = ThreeCardPokerHand(remaining_deck)
    ante_result, pairplus_result, sixcard_result, sixcard_hand = None, None, None, None
    exposed_card_rank = banker_hand.hand[0] % 13

    if player_hand >= PLAYER_QUALIFY[exposed_card_rank]:
        if banker_hand >= BANKER_QUALIFY:
            if player_hand == banker_hand:
                ante_result = BET_RESULT_TIE
            elif player_hand > banker_hand:
                ante_result = BET_RESULT_WIN
            else:
                ante_result = BET_RESULT_LOSE
        else:
            ante_result = BET_RESULT_DQ
        #  PairPlus
        pairplus_result = BET_RESULT_WIN if player_hand.value != HIGH_CARD else BET_RESULT_LOSE

        # SixCard
        for five_card_hand in combinations(player_hand.hand + banker_hand.hand, 5):
            five_card_hand_info = hand_info(five_card_hand)
            for index, value in enumerate(FIVE_CARD_VALUE_ORDER):
                if value.test(five_card_hand_info):
                    sixcard_hand = min(index, sixcard_hand) if sixcard_hand is not None else index
        sixcard_result = BET_RESULT_WIN if sixcard_hand is not None else BET_RESULT_LOSE
    else:
        ante_result = BET_RESULT_FOLD

    return player_hand, ante_result, pairplus_result, sixcard_result, sixcard_hand


class ThreeCardBank(object):
    PAIRPLUS_PAYOUTS = {
        PAIR: 1, FLUSH: 3, STRAIGHT: 6, THREE_OF_A_KIND: 30,
        STRAIGHT_FLUSH: 40, ROYAL_FLUSH: 200}
    SIXCARD_PAYOUTS = {
        THREE_OF_A_KIND: 7, STRAIGHT: 10, FLUSH: 15, FULL_HOUSE: 20,
        FOUR_OF_A_KIND: 100, STRAIGHT_FLUSH: 200, ROYAL_FLUSH: 1000}
    BANKER_FEE = 3
    PLAYER_FEE = 1

    def __init__(self):
        self.balance = 0
        self.low = 0
        self.high = 0

    @staticmethod
    def net_payout(ante_result, pairplus_result, sixcard_result, player, three_card_hand_val, five_card_hand_val):
        if ante_result == BET_RESULT_FOLD:
            return -(player.ante + player.pairplus + player.sixcard)

        net_payout_amount = 0
        if ante_result == BET_RESULT_DQ:
            net_payout_amount += player.ante
        elif ante_result == BET_RESULT_WIN:
            net_payout_amount += player.ante * 2
        elif ante_result == BET_RESULT_LOSE:
            net_payout_amount -= player.ante * 2

        if pairplus_result == BET_RESULT_WIN:
            net_payout_amount += player.pairplus * ThreeCardBank.PAIRPLUS_PAYOUTS[three_card_hand_val]
        else:
            net_payout_amount -= player.pairplus

        if sixcard_result == BET_RESULT_WIN:
            #print "Payout: {}".format(player.sixcard * ThreeCardBank.SIXCARD_PAYOUTS[FIVE_CARD_VALUE_ORDER[five_card_hand_val]])
            net_payout_amount += player.sixcard * ThreeCardBank.SIXCARD_PAYOUTS[FIVE_CARD_VALUE_ORDER[five_card_hand_val]]
        else:
            net_payout_amount -= player.sixcard

        return net_payout_amount

    def update(self, ante_result, pairplus_result, sixcard_result, player,
               three_card_hand_val, five_card_hand_val, banking=True):
        payout = ThreeCardBank.net_payout(
            ante_result, pairplus_result, sixcard_result,
            player, three_card_hand_val, five_card_hand_val)
        self.balance -= payout if banking else -1 * payout
        self.low = min(self.balance, self.low)
        self.high = max(self.balance, self.high)

    def update_fee(self, banking):
        self.balance -= self.BANKER_FEE if banking else self.PLAYER_FEE

    def print_bank(self):
        print "Bank:  ${:,.2f} - Low:  ${:,.2f} - High:  ${:,.2f}".format(self.balance, self.low, self.high)


class PlayerHandResultsTracker(object):
    def __init__(self):
        FLUSH_OR_BETTER = ThreeCardPokerHand.VALUE_ORDER[TC_FLUSH:]
        self.player_hand_results = {h.name: {hr: 0 for hr in TRACKED_RESULTS.keys()} for h in FLUSH_OR_BETTER}
        self.player_fold_count = 0

    def track_results(self, player_hand, ante_result):
        # Track player hand results for verification
        value_rank = ThreeCardPokerHand.VALUE_ORDER.index(player_hand.value)
        # The odds here: http://wizardofodds.com/games/three-card-poker/
        # roll up results of hands FLUSH or less into a single group.
        # Doing that as well
        if ante_result == BET_RESULT_FOLD:
            self.player_fold_count += 1
        else:
            hand_name = player_hand.value.name if value_rank >= TC_FLUSH else FLUSH.name
            self.player_hand_results[hand_name][ante_result] += 1

    def print_results(self):
        print "PLAYER#########################################"
        for hand_value_name, result_set in self.player_hand_results.items():
            print '{}:'.format(hand_value_name)
            for result, qty in result_set.items():
                print '    {}: {}% ({})'.format(TRACKED_RESULTS[result], round(qty / NUM_ROUNDS, 4) * 100, qty)
        print 'Player Folds: {}% ({})'.format(round(self.player_fold_count / NUM_ROUNDS, 4) * 100, self.player_fold_count)

if __name__ == '__main__':
    """
    A 52 card deck.
    The first 13 ints (0-12, inclusive) represent 2-Ace of Spades,
    the next 13 represent 2-Ace of Clubs and so on.
    """

    ACE, DECK = 12, set(range(52))
    ACE_OFF = ACE + 13
    KING, QUEEN, JACK, TEN, NINE, EIGHT, SEVEN, SIX, FIVE, FOUR, THREE, TWO = [ACE-x for x in range(1, 13)]
    KING_OFF, QUEEN_OFF, JACK_OFF, TEN_OFF, NINE_OFF, EIGHT_OFF, SEVEN_OFF, SIX_OFF, FIVE_OFF, FOUR_OFF, THREE_OFF, TWO_OFF = [ACE_OFF-x for x in range(1, 13)]

    NUM_ROUNDS = 200
    NUM_TRIALS = 1e4
    UPDATE_THRESHOLD = NUM_TRIALS / 10 or 1

    BANKER_QUALIFY = ThreeCardPokerHand([TWO, THREE, QUEEN_OFF])
    PLAYER_QUALIFY = {x: ThreeCardPokerHand([FIVE,  NINE, TEN_OFF]) for x in range(TWO, JACK) }
    PLAYER_QUALIFY.update({
        ACE: ThreeCardPokerHand([TWO, NINE, ACE_OFF]),
        KING: ThreeCardPokerHand([TWO, NINE, KING_OFF]),
        QUEEN: ThreeCardPokerHand([TWO, NINE, QUEEN_OFF]),
        JACK: ThreeCardPokerHand([SIX, SEVEN, JACK_OFF])})

    TC_FLUSH = ThreeCardPokerHand.VALUE_ORDER.index(FLUSH)
    TC_FLUSH = ThreeCardPokerHand.VALUE_ORDER.index(FLUSH)

    banker_betting_patterns = [
        ThreeCardPokerBet(ante=50, pairplus=0, sixcard=0),
        ThreeCardPokerBet(ante=50, pairplus=10, sixcard=5),
        ThreeCardPokerBet(ante=100, pairplus=0, sixcard=0),
        ThreeCardPokerBet(ante=35, pairplus=5, sixcard=0)]
    banker_betting_patterns = banker_betting_patterns[-1:]

    BORING_PLAYER = ThreeCardPokerBet(ante=20, pairplus=10, sixcard=5)
    AVG_PLAYER = ThreeCardPokerBet(ante=35, pairplus=20, sixcard=15)
    players_sets = [
        [ThreeCardPokerBet(ante=100, pairplus=35, sixcard=20)] +
        [AVG_PLAYER for x in range(2)] +
        [ThreeCardPokerBet(ante=15, pairplus=10, sixcard=5)
         for x in range(2)],
    ]

    with open('threecard.txt', 'w+') as results_file:
        for banker_bet in banker_betting_patterns:
            for players in players_sets:
            #with open('threecard_3-20-10-5-{}_{}_{}.csv'.format(banker_bet.ante, banker_bet.pairplus, banker_bet.sixcard), 'w+') as raw_file:  # noqa
                run_results = []
                print "Banker Bet {} {} {}!!!!!!!!!!!!!!!!\n".format(banker_bet.ante, banker_bet.pairplus, banker_bet.sixcard) * 5
                for run in range(int(NUM_TRIALS)):
                    bank = ThreeCardBank()
                    tracker = PlayerHandResultsTracker()

                    def play_hand(banker_hand, remaining_deck, player, banking, bank, tracker):
                        player_hand, ante_result, pairplus_result, sixcard_result, sixcard_hand = play_three_card(banker_hand, remaining_deck, player)
                        bank.update(ante_result, pairplus_result, sixcard_result, player, player_hand.value, sixcard_hand, banking=banking)
                        tracker.track_results(player_hand, ante_result)
                        player_hand, ante_result, pairplus_result, sixcard_result, sixcard_hand
                        return player_hand

                    for x in range(int(NUM_ROUNDS)):
                        banker_hand = ThreeCardPokerHand(DECK)
                        deal_qualifies = banker_hand >= BANKER_QUALIFY
                        remaining_deck = DECK - set(banker_hand.hand)

                        # Bank half
                        if x % 2 == 0:
                            for player in players:
                                player_hand = play_hand(banker_hand, remaining_deck, player, True, bank, tracker)
                                remaining_deck = remaining_deck - set(player_hand.hand)
                            bank.update_fee(banking=True)
                        else:
                            play_hand(banker_hand, remaining_deck, banker_bet, False, bank, tracker)
                            bank.update_fee(banking=False)
                    if run % UPDATE_THRESHOLD == 0:
                        print('{}% complete'.format(run / NUM_TRIALS * 100))
                    run_results.append((bank.balance, bank.low))

                BET_DESC = '{}/{}/{}'
                wager_groups = {}
                for p in players:
                    bet_desc_text = BET_DESC.format(p.ante, p.pairplus, p.sixcard)
                    wager_groups[bet_desc_text] = wager_groups.get(bet_desc_text, 0) + 1

                player_wager_desc = ''
                for g in sorted(wager_groups.keys()):
                    player_wager_desc += '  {} Player/s betting {}\n'.format(wager_groups[g], g)
                bal = [b for b, l in run_results]
                low = [l for b, l in run_results]

                M_LOW = int(NUM_TRIALS) / 2 - 1
                M_HIGH = M_LOW + 2
                results_file.write(
                    "#################################################\n"
                    "Banker Wagers:\n  Ante: {}, PairPlus: {}, Six Card: {}\n"
                    "Player Wagers:\n{}\n"
                    "Mean Balance: {}\nMedian Balance: {}\nWorst Loss: {}"
                    "\nWorst Ending Balance: {}\nBiggest Win: {}\n\n".format(
                        banker_bet.ante, banker_bet.pairplus, banker_bet.sixcard,
                        player_wager_desc,
                        sum(bal)/len(bal), sum(sorted(bal)[M_LOW:M_HIGH])/2, min(low), min(bal),
                        max(bal)))

                results_file.write("Cumulative Profit:\n")
                breakdown = {x: 0 for x in range(-5000, 8500, 500)}
                for b in bal:
                    for limit in breakdown.keys():
                        if b < limit:
                            breakdown[limit] += 1
                for limit in sorted(breakdown.keys()):
                    results_file.write(
                        "{}: {}%\n".format(
                        limit, round(breakdown[limit]*1.0/NUM_TRIALS * 100, 2)))

                results_file.write("\n\nCumulative Max Loss:\n")
                breakdown = {x: 0 for x in range(-10000, 0, 500)}
                for l in low:
                    for limit in breakdown.keys():
                        if l < limit:
                            breakdown[limit] += 1
                for limit in sorted(breakdown.keys()):
                    results_file.write(
                        "{}: {}%\n".format(
                        limit, round(breakdown[limit]*1.0/NUM_TRIALS * 100, 2)))
                results_file.write("#################################################\n\n\n")
