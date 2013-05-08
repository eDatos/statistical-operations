package org.siemac.metamac.statistical.operations.core.serviceimpl.utils;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.core.common.serviceimpl.utils.ValidationUtils;
import org.siemac.metamac.core.common.util.TimeUtils;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionParameters;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;

public class CheckMandatoryMetadataUtil {

    // ----------------------------------------------------------------------------------------------------------------
    // ---------------------------------------------- FAMILY VALIDATIONS ----------------------------------------------
    // ----------------------------------------------------------------------------------------------------------------

    public static void checkCreateFamily(Family family) throws MetamacException {
        checkCreateFamily(family, null);
    }

    /**
     * Check family mandatory metadata for create it
     * 
     * @param family
     * @throws MetamacException
     */
    public static void checkCreateFamily(Family family, List<MetamacExceptionItem> exceptions) throws MetamacException {
        if (exceptions == null) {
            exceptions = new ArrayList<MetamacExceptionItem>();
        }

        ValidationUtils.checkMetadataRequired(family.getCode(), ServiceExceptionParameters.FAMILY_CODE, exceptions);
        ValidationUtils.checkMetadataRequired(family.getUrn(), ServiceExceptionParameters.FAMILY_URN, exceptions);
        ValidationUtils.checkMetadataRequired(family.getTitle(), ServiceExceptionParameters.FAMILY_TITLE, exceptions);
        ValidationUtils.checkMetadataRequired(family.getProcStatus(), ServiceExceptionParameters.FAMILY_PROC_STATUS, exceptions);
        ValidationUtils.checkSemanticIdentifierAsMetamacID(family.getCode(), ServiceExceptionParameters.FAMILY_CODE, exceptions);

        if (!exceptions.isEmpty()) {
            throw new MetamacException(exceptions);
        }
    }

    /**
     * Check family mandatory metadata for publish it internally
     * 
     * @param family
     * @throws MetamacException
     */

    public static void checkFamilyForPublishInternally(Family family) throws MetamacException {
        checkFamilyForPublishInternally(family, null);
    }

    public static void checkFamilyForPublishInternally(Family family, List<MetamacExceptionItem> exceptions) throws MetamacException {
        if (exceptions == null) {
            exceptions = new ArrayList<MetamacExceptionItem>();
        }

        ValidationUtils.checkMetadataRequired(family.getInternalInventoryDate(), ServiceExceptionParameters.FAMILY_INTERNAL_INVENTORY_DATE, exceptions);

        checkCreateFamily(family);
    }

    /**
     * Check family mandatory metadata for publish it externally
     * 
     * @param family
     * @throws MetamacException
     */
    public static void checkFamilyForPublishExternally(Family family) throws MetamacException {
        List<MetamacExceptionItem> exceptions = new ArrayList<MetamacExceptionItem>();

        ValidationUtils.checkMetadataRequired(family.getInventoryDate(), ServiceExceptionParameters.FAMILY_INVENTORY_DATE, exceptions);

        checkFamilyForPublishInternally(family, exceptions);
    }

    // -------------------------------------------------------------------------------------------------------------------
    // ---------------------------------------------- OPERATION VALIDATIONS ----------------------------------------------
    // -------------------------------------------------------------------------------------------------------------------

    public static void checkCreateOperation(Operation operation) throws MetamacException {
        checkCreateOperation(operation, null);
    }

    /**
     * Check operation mandatory metadata for create it
     * 
     * @param operation
     * @throws MetamacException
     */
    public static void checkCreateOperation(Operation operation, List<MetamacExceptionItem> exceptions) throws MetamacException {
        if (exceptions == null) {
            exceptions = new ArrayList<MetamacExceptionItem>();
        }

        ValidationUtils.checkMetadataRequired(operation.getCode(), ServiceExceptionParameters.OPERATION_CODE, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getUrn(), ServiceExceptionParameters.OPERATION_URN, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getTitle(), ServiceExceptionParameters.OPERATION_TITLE, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getProcStatus(), ServiceExceptionParameters.OPERATION_PROC_STATUS, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getStatus(), ServiceExceptionParameters.OPERATION_STATUS, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getSubjectArea(), ServiceExceptionParameters.OPERATION_SUBJECT_AREA, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getIndicatorSystem(), ServiceExceptionParameters.OPERATION_INDICATOR_SYSTEM, exceptions);

        if (operation.getReleaseCalendar().equals(false)) {
            ValidationUtils.checkMetadataEmpty(operation.getReleaseCalendarAccess(), ServiceExceptionParameters.OPERATION_RELEASE_CALENDAR_ACCESS, exceptions);
        } else {
            ValidationUtils.checkMetadataRequired(operation.getReleaseCalendarAccess(), ServiceExceptionParameters.OPERATION_RELEASE_CALENDAR_ACCESS, exceptions);
            ValidationUtils.validateUrl(operation.getReleaseCalendarAccess(), ServiceExceptionParameters.OPERATION_RELEASE_CALENDAR_ACCESS, exceptions);
        }

        ValidationUtils.checkSemanticIdentifierAsMetamacID(operation.getCode(), ServiceExceptionParameters.OPERATION_CODE, exceptions);

        if (!exceptions.isEmpty()) {
            throw new MetamacException(exceptions);
        }
    }

    public static void checkOperationForPublishInternally(Operation operation) throws MetamacException {
        checkOperationForPublishInternally(operation, null);
    }

    /**
     * Check operation mandatory metadata for publish it internally
     * 
     * @param operationDto
     * @throws MetamacException
     */
    public static void checkOperationForPublishInternally(Operation operation, List<MetamacExceptionItem> exceptions) throws MetamacException {

        if (exceptions == null) {
            exceptions = new ArrayList<MetamacExceptionItem>();
        }

        ValidationUtils.checkMetadataRequired(operation.getCommonMetadata(), ServiceExceptionParameters.OPERATION_COMMON_METADATA, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getObjective(), ServiceExceptionParameters.OPERATION_OBJECTIVE, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getSurveyType(), ServiceExceptionParameters.OPERATION_SURVEY_TYPE, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getOfficialityType(), ServiceExceptionParameters.OPERATION_OFFICIALITY_TYPE, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getProducer(), ServiceExceptionParameters.OPERATION_PRODUCER, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getRegionalResponsible(), ServiceExceptionParameters.OPERATION_REGIONAL_RESPONSIBLE, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getInternalInventoryDate(), ServiceExceptionParameters.OPERATION_INTERNAL_INVENTORY_DATE, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getCurrentlyActive(), ServiceExceptionParameters.OPERATION_CURRENTLY_ACTIVE, exceptions);
        ValidationUtils.checkMetadataRequired(operation.getPublisher(), ServiceExceptionParameters.OPERATION_PUBLISHER, exceptions);

        checkCreateOperation(operation, exceptions);
    }

    /**
     * Check operation mandatory metadata for publish it externally
     * 
     * @param operation
     * @throws MetamacException
     */
    public static void checkOperationForPublishExternally(Operation operation) throws MetamacException {

        List<MetamacExceptionItem> exceptions = new ArrayList<MetamacExceptionItem>();

        ValidationUtils.checkMetadataRequired(operation.getInventoryDate(), ServiceExceptionParameters.OPERATION_INVENTORY_DATE, exceptions);

        checkOperationForPublishInternally(operation, exceptions);
    }

    // ------------------------------------------------------------------------------------------------------------------
    // ---------------------------------------------- INSTANCE VALIDATIONS ----------------------------------------------
    // ------------------------------------------------------------------------------------------------------------------

    /**
     * Check instance mandatory metadata for create it
     * 
     * @param instance
     * @throws MetamacException
     */
    public static void checkCreateInstance(Instance instance, List<MetamacExceptionItem> exceptions) throws MetamacException {
        if (exceptions == null) {
            exceptions = new ArrayList<MetamacExceptionItem>();
        }

        ValidationUtils.checkMetadataRequired(instance.getOrder(), ServiceExceptionParameters.INSTANCE_ORDER, exceptions);
        ValidationUtils.checkMetadataRequired(instance.getUrn(), ServiceExceptionParameters.INSTANCE_URN, exceptions);
        ValidationUtils.checkMetadataRequired(instance.getCode(), ServiceExceptionParameters.INSTANCE_CODE, exceptions);
        ValidationUtils.checkMetadataRequired(instance.getTitle(), ServiceExceptionParameters.INSTANCE_TITLE, exceptions);
        ValidationUtils.checkMetadataRequired(instance.getProcStatus(), ServiceExceptionParameters.INSTANCE_PROC_STATUS, exceptions);

        if (instance.getBasePeriod() != null && !TimeUtils.isTimeValue(instance.getBasePeriod())) {
            exceptions.add(new MetamacExceptionItem(ServiceExceptionType.METADATA_INCORRECT, ServiceExceptionParameters.INSTANCE_BASE_PERIOD));
        }

        ValidationUtils.checkSemanticIdentifierAsMetamacID(instance.getCode(), ServiceExceptionParameters.INSTANCE_CODE, exceptions);

        if (!exceptions.isEmpty()) {
            throw new MetamacException(exceptions);
        }
    }

    /**
     * Check instance mandatory metadata for publish it internally
     * 
     * @param instance
     * @throws MetamacException
     */
    public static void checkInstanceForPublishInternally(Instance instance) throws MetamacException {
        List<MetamacExceptionItem> exceptions = new ArrayList<MetamacExceptionItem>();

        ValidationUtils.checkMetadataRequired(instance.getInstanceType(), ServiceExceptionParameters.INSTANCE_INSTANCE_TYPE, exceptions);
        ValidationUtils.checkMetadataRequired(instance.getInternalInventoryDate(), ServiceExceptionParameters.INSTANCE_INTERNAL_INVENTORY_DATE, exceptions);

        checkCreateInstance(instance, exceptions);
    }

    /**
     * Check instance mandatory metadata for publish it externally
     * 
     * @param instanceDto
     * @throws MetamacException
     */
    public static void checkInstanceForPublishExternally(Instance instance) throws MetamacException {
        List<MetamacExceptionItem> exceptions = new ArrayList<MetamacExceptionItem>();

        ValidationUtils.checkMetadataRequired(instance.getInventoryDate(), ServiceExceptionParameters.INSTANCE_INVENTORY_DATE, exceptions);
        checkInstanceForPublishInternally(instance);
    }

    public static void checkCreateInstance(Instance instance) throws MetamacException {
        checkCreateInstance(instance, null);
    }
}
