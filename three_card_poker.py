import random

from collections import namedtuple


"""
A 52 card deck.
The first 13 ints (0-12, inclusive) represent 2-Ace of Spades,
the next 13 represent 2-Ace of Clubs and so on.
"""
DECK = range(52)

ACE = 12  # For code readiblity. Value has nothing to do with hand values


def ranks_by_count(hand_info, count):
    return [card for card, qty in hand_info['rank_counts'] if qty == count]


def high_card_tie_breaker(hand_info_1, hand_info_2):
    sorted_1 = sorted(ranks_by_count(hand_info_1, 1), reverse=True)
    sorted_2 = sorted(ranks_by_count(hand_info_2, 1), reverse=True)

    for idx, hand_1_card in enumerate(sorted_1):
        if hand_1_card == sorted_2['idx']:
            continue
        return hand_info_1 if hand_1_card > sorted_2['idx'] else hand_info_2
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


Hand = namedtuple('Hand', ['hand', 'test', 'tie_breaker'])

ROYAL_FLUSH = Hand(hand='Royal Flush', test=lambda info: info['is_royal_flush'], tie_breaker=high_card_tie_breaker)
STRAIGHT_FLUSH = Hand(hand='Straight Flush', test=lambda info: info['is_flush'] and info['is_straight'], tie_breaker=high_card_tie_breaker)
FOUR_OF_A_KIND = Hand(hand='Four of a Kind', test=lambda info: info['is_four_of_a_kind'], tie_breaker=four_of_a_kind_tie_breaker)
FULL_HOUSE = Hand(hand='Full House', test=lambda info: info['is_full_house'], tie_breaker=full_house_tie_breaker)
FLUSH = Hand(hand='Flush', test=lambda info: info['is_flush'], tie_breaker=high_card_tie_breaker)
STRAIGHT = Hand(hand='Straight', test=lambda info: info['is_straight'], tie_breaker=high_card_tie_breaker)
THREE_OF_A_KIND = Hand(hand='Three of a Kind', test=lambda info: info['is_three_of_a_kind'],  tie_breaker=three_of_a_kind_tie_breaker)
TWO_PAIR = Hand(hand='Two Pair', test=lambda info: info['is_two_pair'], tie_breaker=two_pair_tie_breaker)
PAIR = Hand(hand='Pair', test=lambda info: info['is_pair'], tie_breaker=pair_tie_breaker)
HIGH_CARD = Hand(hand='High Card', test=lambda info: True, tie_breaker=high_card_tie_breaker)


hand_order_three_card = (ROYAL_FLUSH, STRAIGHT_FLUSH, THREE_OF_A_KIND, STRAIGHT, FLUSH, PAIR, HIGH_CARD)


def hand_info(hand):
    ranks = sorted([card % 13 for card in hand])
    suits = [card / 13 for card in hand]
    is_straight = ranks in ([ranks[0] + x for x in range(len(hand))], [x for x in range(len(hand)-1)] + [ACE])

    is_flush = not [s for s in suits if s != suits[0]]
    is_pair = is_two_pair = is_three_of_a_kind = is_full_house = is_four_of_a_kind = is_straight_flush = is_royal_flush = False

    # A straight and flush ending in ace is a royal
    if is_flush and is_straight:
        is_straight_flush = True
        if ranks == [ACE-2, ACE-1, ACE]:
            is_royal_flush = True

    rank_counts = {}
    # If it's a straight, it cannot be boat/pair/2pr/3kind
    if not is_straight:
        for card in ranks:
            rank_counts[card] = rank_counts.setdefault(card, 0) + 1
        pairs = [card for card, count in rank_counts.items() if count >= 2]
        is_pair = bool(pairs)
        is_two_pair = len(pairs) > 1

        triples = [card for card, count in rank_counts.items() if count >= 3]
        is_three_of_a_kind = bool(triples)

        is_full_house = bool(bool(set(pairs) - set(triples)) and triples)
        is_four_of_a_kind = [card for card, count in rank_counts.items() if count >= 4]
    return locals()


def determine_three_card_hand(hand):
    hand_facts = hand_info(hand)
    for hand in hand_order_three_card:
        if hand.test(hand_facts):
            return hand


def pick_winner_three_card(banker, player):
    banker_hand_determined = determine_three_card_hand(banker)
    player_hand_determined = determine_three_card_hand(player)

    if banker_hand_determined == player_hand_determined:
        return banker_hand_determined.tie_breaker(banker, player)

    return banker if hand_order_three_card.index(banker_hand_determined) < hand_order_three_card.index(player_hand_determined) else player


if __name__ == '__main__':
    hand_results = {hand.hand: 0 for hand in hand_order_three_card}
    for x in range(1000000):
        hand_results[determine_three_card_hand(random.sample(DECK, 3)).hand] += 1

    for hand, count in hand_results.items():
        print hand + ' : ' + str(round(count / 1000000.0, 8))
