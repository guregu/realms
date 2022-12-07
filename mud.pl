:- module(mud, [item/2, item_description/2, item_value/2, item_weight/2, item_element/2, item_rarity/2, item_power/2]).

:- use_module(parser).
:- use_module(world).

% item_value(ItemID, Value).
item_value(1, 5).
item_value(2, 10).
item_value(3, 7).
item_value(4, 12).

% item_weight(ItemID, Weight).
item_weight(1, 0.5).
item_weight(2, 2.0).
item_weight(3, 3.5).
item_weight(4, 1.0).

% item(ItemID, Item).
item(1, "The Flamekeeper's Blade").
item(2, "The Skycaller's Bow").
item(3, "The Earthshaker's Axe").
item(4, "The Ocean's Herald").
item(5, "The Sorcerer's Wand").
item(6, "The Frostweaver's Staff").
item(7, "The Shadowstalker's Blade").
item(8, "The Infernal War-mace").
item(9, "The Hammer of the Heavens").
item(10, "The Assassin's Talon").
item(11, "The Cosmic Scythe").

% item_element(ItemID, Element).
item_element(1, fire).
item_element(2, lightning).
item_element(3, earth).
item_element(4, water).
item_element(5, arcane).
item_element(6, ice).
item_element(7, shadow).
item_element(8, fire).
item_element(9, lightning).
item_element(10, shadow).
item_element(11, space).

% rarity(Name, Rank).
rarity(common, 1).
rarity(uncommon, 2).
rarity(rare, 3).
rarity(epic, 4).
rarity(legendary, 5).

% item_rarity(ItemID, Rarity).
item_rarity(1, legendary).
item_rarity(2, epic).
item_rarity(3, rare).
item_rarity(4, uncommon).
item_rarity(5, rare).
item_rarity(6, uncommon).
item_rarity(7, legendary).
item_rarity(8, epic).
item_rarity(9, legendary).
item_rarity(10, rare).
item_rarity(11, legendary).

% item_power(ItemID, Power).
item_power(1, 75).
item_power(2, 65).
item_power(3, 90).
item_power(4, 70).
item_power(5, 85).
item_power(6, 80).
item_power(7, 75).
item_power(8, 90).
item_power(9, 95).
item_power(10, 60).
item_power(11, 85).

% item_description(ItemID, Description).
item_description(1, "A weapon crafted by the great fire spirits of the underworld, its flames burn with the very essence of life and death. In the hands of a skilled warrior, it is said to be able to cut through even the strongest of defenses and ignite the very soul of its target.").
item_description(2, "A powerful longbow crafted from the bones of the great dragons of the sky. Its arrows are said to be able to pierce even the strongest of armor and call forth the fury of the storm, raining down lightning and thunder on the wielder's foes.").
item_description(3, "A massive double-bladed axe crafted from the very stones of the earth. In the hands of a skilled warrior, it is said to be able to split the very ground beneath the wielder's feet and unleash devastating earthquakes on the battlefield.").
item_description(4, "A trident wielded by the great sea gods of old. Its power is said to be able to call forth the tides and the fury of the ocean to wash away the wielder's foes, drowning them in a torrent of water and foam.").
item_description(5, "A powerful magical wand crafted by the most ancient and powerful of wizards. Its magic is said to be able to bend the very laws of the universe to the wielder's will, allowing them to cast powerful spells and incantations with ease.").
item_description(6, "A staff crafted from the frozen hearts of the ancient ice giants. Its power is said to be able to freeze even the strongest of warriors in their tracks, encasing them in layers of ice and snow.").
item_description(7, "A long, curved sword crafted from the very essence of darkness. Its power is said to be able to strike from the shadows, consuming the souls of those it strikes and leaving them forever lost in the darkness.").
item_description(8, "A massive spiked mace wielded by the greatest warriors of the underworld. Its power is said to be able to summon the fires of hell and lay waste to the wielder's enemies, burning them to ashes with a single strike.").
item_description(9, "A divine weapon wielded by the gods themselves. Its power is said to be able to call forth the very storms of the sky and the wrath of the gods to smite the wielder's foes, leaving them trembling in fear before the might of the heavens.").
item_description(10, "A sleek and deadly dagger crafted from the very shadows of the night.").
item_description(11, "A massive scythe that seems to be made of pure, shimmering energy. In the hands of a skilled wielder, it is said to be able to cut through the very fabric of space and time, allowing them to move freely between different dimensions and realities.").

% element(Element).
element(air).
element(darkness).
element(death).
element(disease).
element(energy).
element(fire).
element(ice).
element(life).
element(light).
element(lightning).
element(nature).
element(poison).
element(shadow).
element(sound).
element(space).
element(stone).
element(thunder).
element(time).
element(water).

room(1).
room(2).
room(3).
room(4).
room(5).

room_name(1, "Radiant Cavern").
room_name(2, "Gleaming Chamber").
room_name(3, "Illuminated Corridor").
room_name(4, "Luminous Hallway").
room_name(5, "Chamber of the Heavens").

room_exit(1, east, 2).
room_exit(2, north, 3).
room_exit(2, west, 1).
room_exit(3, east, 4).
room_exit(3, south, 2).
room_exit(4, north, 5).
room_exit(4, west, 3).
room_exit(5, south, 4).

room_item(3, 9). % hammer

room_description(1, "A large, brightly-lit cavern filled with the warmth and glow of the sun. The walls are smooth and polished, and a golden light can be seen coming from an opening to the east.").
room_description(2, "A chamber filled with the dazzling light of a thousand stars. The walls are adorned with precious gems and crystals, and the air is filled with a soft, ethereal glow. A narrow passage leads north.").
room_description(3, "A long, winding corridor filled with the gentle light of the moon. The walls and ceiling are smooth and polished, and the floor is covered with a soft, glowing carpet. A faint light can be seen coming from an opening to the east.").
room_description(4, "A wide hallway filled with the gentle glow of a thousand candles. The walls are adorned with ancient carvings of the gods of light, and the floor is covered with a soft, glowing carpet. A faint light can be seen coming from an opening to the north.").
room_description(5, "A grand chamber filled with the radiance of the sun. The walls are adorned with shimmering gold and silver, and the floor is covered with a soft, glowing carpet. In the center of the room, a brilliant sword rests on a pedestal, its blade shining with the light of the heavens.").

room_detail(1, "archway", "The archway to the east is a wide, golden structure, glowing with the light of the sun. It seems to radiate warmth and energy, inviting travelers to step through and explore the mysteries beyond.").
room_detail(2, "walls", "The walls of the chamber are adorned with precious gems and crystals, sparkling and shining in the light. The gems seem to pulsate with energy, as if they are alive with the light of the stars.").
room_detail(2, "carpet", "The floor of the chamber is covered with a soft, glowing carpet, providing a comfortable and inviting surface to walk on. The carpet seems to shimmer and shift beneath your feet, as if it is made of pure, ethereal light.").
room_detail(2, "passage", "The narrow passage to the north is dark and mysterious, leading into unknown depths. A faint breeze can be felt emanating from the passage, carrying with it the distant sound of thunder.").
room_detail(3, "archway", "The archway to the east is a wide, golden structure, glowing with the light of the moon. It seems to radiate tranquility and peace, inviting travelers to step through and explore the mysteries beyond.").
room_detail(4, "carvings", "The walls of the hallway are adorned with ancient carvings of the gods of light, their faces serene and peaceful. The carvings seem to radiate a sense of divine power, as if the gods themselves are watching over the chamber.").
room_detail(4, "carpet", "The floor of the hallway is covered with a soft, glowing carpet, providing a comfortable and inviting surface to walk on. The carpet seems to shimmer and shift beneath your feet, as if it is made of pure, radiant light.").
room_detail(4, "archway", "To the north, a grand golden archway towers over the room, glowing with the light of a thousand candles. It seems to radiate warmth and comfort, inviting travelers to step through and explore the mysteries beyond.").
room_detail(5, "sword", "The sword in the center of the room is a beautiful, radiant weapon, its blade shining with the light of the heavens. It is the Hammer of the Heavens, a powerful weapon said to be able to smite the enemies of the gods. It rests on a pedestal of pure, shining gold, beckoning to be wielded by a brave and worthy hero.").
room_detail(5, "walls", "The walls of the chamber are adorned with shimmering gold and silver, reflecting the light of the sun. The gold and silver seem to radiate a sense of wealth and prosperity, as if the chamber itself is a treasure trove of unimaginable riches.").
room_detail(5, "carpet", "The floor of the chamber is covered with a soft, glowing carpet, providing a comfortable and inviting surface to walk on. The carpet seems to shimmer and shift beneath your feet, as if it is made of pure, radiant light.").

