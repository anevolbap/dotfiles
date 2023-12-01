/*
Copyright 2019 @foostan
Copyright 2020 Drashna Jaelre <@drashna>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include QMK_KEYBOARD_H
#ifdef RAW_ENABLE
#    include "raw_hid.h"
#endif

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [0] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,-----------------------------------------------------.
       KC_TAB,    KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,                         KC_Y,    KC_U,    KC_I,    KC_O,   KC_P,  KC_BSPC,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      KC_LCTL,    KC_A,    KC_S,    KC_D,    KC_F,    KC_G,                         KC_H,    KC_J,    KC_K,    KC_L, KC_SCLN, KC_QUOT,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      KC_LSFT,    KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                         KC_N,    KC_M, KC_COMM,  KC_DOT, KC_SLSH,  KC_ESC,
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|
                                          KC_LGUI,   MO(1),  KC_SPC,     KC_ENT,   MO(2), KC_LALT
                                      //`--------------------------'  `--------------------------'

  ),

    [1] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,-----------------------------------------------------.
       KC_TAB,    KC_1,    KC_2,    KC_3,    KC_4,    KC_5,                         KC_6,    KC_7,    KC_8,    KC_9,    KC_0, KC_BSPC,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      KC_LCTL, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                      KC_LEFT, KC_DOWN,   KC_UP,KC_RIGHT, XXXXXXX, XXXXXXX,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      KC_LSFT, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|
                                          KC_LGUI, _______,  KC_SPC,     KC_ENT,   MO(3), KC_RALT
                                      //`--------------------------'  `--------------------------'
  ),

    [2] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,-----------------------------------------------------.
       KC_TAB, KC_EXLM,   KC_AT, KC_HASH,  KC_DLR, KC_PERC,                      KC_CIRC, KC_AMPR, KC_ASTR, KC_LPRN, KC_RPRN, KC_BSPC,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      KC_LCTL, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                      KC_MINS,  KC_EQL, KC_LBRC, KC_RBRC, KC_BSLS,  KC_GRV,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      KC_LSFT, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                      KC_UNDS, KC_PLUS, KC_LCBR, KC_RCBR, KC_PIPE, KC_TILD,
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|
                                          KC_LGUI,   MO(3),  KC_SPC,     KC_ENT, _______, KC_RALT
                                      //`--------------------------'  `--------------------------'
  ),

    [3] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,-----------------------------------------------------.
        QK_BOOT, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
       UG_TOGG, UG_HUEU, UG_SATU, UG_VALU, XXXXXXX, XXXXXXX,                      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
       UG_NEXT, UG_HUED, UG_SATD, UG_VALD, XXXXXXX, XXXXXXX,                      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|
                                          KC_LGUI, _______,  KC_SPC,     KC_ENT, _______, KC_RALT
                                      //`--------------------------'  `--------------------------'
  )
};

// ---------------------------------------------------------------------------
// Claude Code attention alert via Raw HID
// ---------------------------------------------------------------------------
// The host sends a 32-byte Raw HID packet with data[0] == 0x01 to start a
// rainbow-blinking alert.  The keyboard blinks for ALERT_BLINKS cycles then
// restores the previous RGB state automatically.
// ---------------------------------------------------------------------------
#ifdef RAW_ENABLE

#define ALERT_BLINKS   20   // number of on/off transitions
#define ALERT_INTERVAL 200  // milliseconds per transition

static bool     alert_active = false;
static uint8_t  alert_count  = 0;
static bool     alert_phase  = false;   // false=blue, true=orange
static uint32_t alert_timer  = 0;

// Saved RGB state to restore after the alert.
static bool    saved_enabled = false;
static uint8_t saved_mode    = 0;
static uint8_t saved_hue     = 0;
static uint8_t saved_sat     = 0;
static uint8_t saved_val     = 0;

void raw_hid_receive(uint8_t *data, uint8_t length) {
    if (data[0] == 0x01 && !alert_active) {
        // Save current state.
        saved_enabled = rgblight_is_enabled();
        saved_mode    = rgblight_get_mode();
        saved_hue     = rgblight_get_hue();
        saved_sat     = rgblight_get_sat();
        saved_val     = rgblight_get_val();

        // Kick off the alert.
        alert_active = true;
        alert_count  = ALERT_BLINKS;
        alert_phase  = false;
        alert_timer  = timer_read32();

        rgblight_enable_noeeprom();
        rgblight_mode_noeeprom(RGBLIGHT_MODE_STATIC_LIGHT);
        rgblight_sethsv_noeeprom(170, 255, 200);  // start blue
    }

    // Always acknowledge.
    uint8_t response[32] = {0};
    response[0] = 0x01;
    raw_hid_send(response, sizeof(response));
}

void housekeeping_task_user(void) {
    if (!alert_active) return;
    if (timer_elapsed32(alert_timer) < ALERT_INTERVAL) return;

    alert_timer = timer_read32();

    alert_phase = !alert_phase;
    rgblight_sethsv_noeeprom(alert_phase ? 21 : 170, 255, 200);  // orange / blue

    if (--alert_count == 0) {
        alert_active = false;
        // Restore previous state.
        if (saved_enabled) {
            rgblight_enable_noeeprom();
            rgblight_mode_noeeprom(saved_mode);
            rgblight_sethsv_noeeprom(saved_hue, saved_sat, saved_val);
        } else {
            rgblight_disable_noeeprom();
        }
    }
}

#endif // RAW_ENABLE
