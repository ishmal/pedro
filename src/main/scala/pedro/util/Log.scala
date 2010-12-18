/**
 * Simple facade for logging
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2010 Bob Jamison
 * 
 *  This file is part of the Pedro library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 3 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */


package pedro.util

import java.util.logging.Logger


object Log
{
    private val logger = Logger.getLogger("Pyx")
    
    def trace(msg: String) = logger.info(msg)

    def trace(fmt: String, args: Any*) = logger.info(fmt.format(args))
    
    def warning(msg: String) = logger.warning(msg)

    def warning(fmt: String, args: Any*) = logger.warning(fmt.format(args))

    def error(msg: String) = logger.severe(msg)

    def error(fmt: String, args: Any*) = logger.severe(fmt.format(args))
    
}

trait Logged
{
    def error(msg: String) =
        Log.error(getClass.getName + " error : " + msg)

    def trace(msg: String) =
        Log.trace(getClass.getName + " : " + msg)
}