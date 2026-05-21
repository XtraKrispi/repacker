'use client'
import { createClient as createSupabaseClient } from "@supabase/supabase-js";

export const createClientWithPasskeyImpl = (url, key) => createSupabaseClient(url, key, {
    auth: {
        experimental: { passkey: true },
    }
});

